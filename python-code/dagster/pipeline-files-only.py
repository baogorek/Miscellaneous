import subprocess

from dagster import MetadataValue, Output, asset, graph, file_relative_path, job
import pandas as pd


@asset
def get_source_data() -> None:
   df = pd.DataFrame({'x': [1, 2, 3], 'y': [4, 5, 6]})
   df.to_csv('my_df.csv', index=False)



@asset(non_argument_deps={"get_source_data"})
def get_r_transformed_data():
    subprocess.check_output(["Rscript", "task-files.R"])
    my_df2 = pd.read_csv('my_df2.csv')
    return Output(
        value = my_df2,
        metadata={
            "num_records": len(my_df2),
            "preview": MetadataValue.md(my_df2.head().to_markdown()),
        }
    )

# How do I clean up the csv files that were dropped?
