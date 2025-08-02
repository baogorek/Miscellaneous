import pandas as pd
from fredapi import Fred
import os
from datetime import date
from dateutil.relativedelta import relativedelta

def get_job_revisions(api_key):
    """
    Fetches and calculates the combined two-month job growth revisions from FRED.

    The calculation replicates the methodology seen in financial news charts: for each
    month's jobs report release, it sums the revision to the prior month's data and
    the revision to the data from two months prior.

    Args:
        api_key (str): Your FRED API key.

    Returns:
        pandas.DataFrame: A DataFrame with the date of the jobs report release
                          and the combined revision in jobs.
    """
    fred = Fred(api_key=api_key)
    # PAYEMS is the FRED series ID for Total Nonfarm Employment
    series_id = 'PAYEMS'
    fred.get_series_info(series_id)
    all_revisions_df = fred.get_series_all_releases(series_id)

    # For printing
    this_month = all_revisions_df.loc[all_revisions_df.realtime_start == pd.to_datetime('2025-08-01')]
    last_month = all_revisions_df.loc[all_revisions_df.realtime_start == pd.to_datetime('2025-07-03')]

    print(this_month)
    print(last_month)

    df = all_revisions_df.copy()
    df['realtime_start'] = pd.to_datetime(df['realtime_start'])
    df['date'] = pd.to_datetime(df['date'])
    
    report_dates = sorted(df['realtime_start'].unique())
    
    revisions_list = []
    
    for i in range(1, len(report_dates)):
        this_month_date = report_dates[i]
        last_month_date = report_dates[i-1]
    
        this_month_df = df[df['realtime_start'] == this_month_date][['date', 'value']].copy()
        last_month_df = df[df['realtime_start'] == last_month_date][['date', 'value']].copy()
    
        this_month_df.rename(columns={'value': 'this_month_value'}, inplace=True)
        last_month_df.rename(columns={'value': 'last_month_value'}, inplace=True)
    
        merged_df = pd.merge(last_month_df, this_month_df, on='date', how='left')
    
        merged_df['revision'] = merged_df['this_month_value'] - merged_df['last_month_value']
        
        merged_df['revision_report_date'] = this_month_date
        merged_df['previous_report_date'] = last_month_date
        
        revisions_list.append(merged_df)

    all_revision_events = pd.concat(revisions_list, ignore_index=True)
    
    # 1. First, remove all rows where a revision could not be calculated.
    clean_revision_events = all_revision_events.dropna(subset=['revision'])
    
    # 2. THEN, from this clean list, find the first revision for each date.
    clean_revision_events = clean_revision_events.sort_values('revision_report_date')
    one_month_revisions = clean_revision_events.drop_duplicates(subset=['date'], keep='first').copy()
    
    one_month_revisions.set_index('date', inplace=True)
    one_month_revisions.to_csv("job_revisions_1_mo.csv", index = False)


print("--- Cleaned One-Month Revisions ---")
print(one_month_revisions[['revision_report_date', 'revision']])

if __name__ == '__main__':
    # --- IMPORTANT ---
    # Replace "YOUR_API_KEY_HERE" with your actual FRED API key.
    # You can also set it as an environment variable named FRED_API_KEY.
    API_KEY = "YOUR_API_KEY_HERE"
    
    if API_KEY == "YOUR_API_KEY_HERE":
        # Check for environment variable as an alternative
        API_KEY = os.getenv('FRED_API_KEY')
        if not API_KEY:
            print("ERROR: Please replace 'YOUR_API_KEY_HERE' with your FRED API key.")
            exit()

    revisions_df = get_job_revisions(api_key=API_KEY)

    if revisions_df is not None:
        # Display the most recent data
        print("\n--- Combined Two-Month Job Revisions ---")
        print(revisions_df.tail(10))

        # Save the data to a CSV file
        output_filename = 'bls_job_revisions_1980_present.csv'
        revisions_df.to_csv(output_filename)
        print(f"\nSuccessfully saved the data to {output_filename}")
        print("You can now use this CSV file for analysis or plotting in Excel, Python, or R.")
