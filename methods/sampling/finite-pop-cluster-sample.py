import numpy as np
import pandas as pd


def generate_population(
    N_households=1000,
    n_districts=10,
    min_size=1,
    max_size=6,
    seed=None
):
    """
    Generate a finite population of households grouped into districts, each with 1-6 persons.
    Assign superpopulation variables y and z at the household level.
    y is correlated with selection probability; z is independent.

    Returns:
        households: DataFrame with columns [household_id, district_id, size, y, z]
        persons: DataFrame with columns [household_id, person_id]
    """
    rng = np.random.default_rng(seed)

    # 1. District sizes in households, summing to N
    # Use multinomial to allocate households to districts
    dist_counts = rng.multinomial(N_households, [1/n_districts]*n_districts)

    # 2. Create households DataFrame
    records = []
    hh_id = 0
    for d_id, hh_count in enumerate(dist_counts, start=1):
        for _ in range(hh_count):
            hh_id += 1
            # household size in persons
            size = rng.integers(min_size, max_size+1)
            records.append({'household_id': hh_id, 'district_id': d_id, 'hh_size': size})
    households = pd.DataFrame.from_records(records)

    # 3. Superpopulation variables at household level
    #   Draw z ~ N(0,1)
    households['z'] = rng.standard_normal(len(households))
    #   Draw latent y* ~ N(0,1)
    # Note: this is interesting because y_latent is something that the designer
    # of the survey knows about but we as analysts don't. So by throwing away
    # the weights, we're throwing away the information about that latent variable,
    # unless we can get it another way
    # It's also a household level variable
    y_latent = rng.standard_normal(len(households))
    #   Define selection probability p ~ logistic(a + b * y_latent)
    #   Choose b to ensure moderate correlation
    b = 1.0
    a = -0.5 * b * households['z'].mean()  # center logistic
    def expit(x):
        return 1 / (1 + np.exp(-x))
    p = expit(a + b * y_latent)
    #   Now set y to be correlated with p; e.g., y = y* + small noise
    households['y'] = y_latent + rng.normal(scale=0.5, size=len(households))

    # 4. Create person-level DataFrame
    person_records = []
    p_id_offset = 0
    for row in households.itertuples():
        for pre_pid in range(1, row.hh_size + 1):
            pid = p_id_offset + pre_pid 
            person_records.append({'household_id': row.household_id, 'person_id': pid})
        p_id_offset = pid
    persons = pd.DataFrame.from_records(person_records)

    # Store selection probability in households for sampling
    households['p_select'] = p / p.sum()

    return households, persons


def sample_households(households, sample_size, seed=None):
    """
    Sample a fixed number of households under a cluster sampling design.

    Args:
        households: DataFrame returned by generate_population()
        sample_size: number of households to sample
        seed: random seed for reproducibility

    Returns:
        sampled_households: DataFrame of sampled households
    """
    rng = np.random.default_rng(seed)
    # Sample household IDs with probability proportional to p_select
    sampled_ids = rng.choice(
        households['household_id'],
        size=sample_size,
        replace=False,
        p=households['p_select']
    )
    sampled = households[households['household_id'].isin(sampled_ids)].copy()
    return sampled


# Example usage:
if __name__ == "__main__":
    # Generate population
    hh, persons = generate_population(seed=123)

    # Draw a sample of 100 households
    sample = sample_households(hh, sample_size=100, seed=456)

