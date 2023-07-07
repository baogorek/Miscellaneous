import subprocess

from dagster import MetadataValue, Output, asset, graph, file_relative_path, job
import pyarrow as pa
import pyarrow.flight
import pandas as pd


@asset
def get_source_data() -> pd.DataFrame:
   df = pd.DataFrame({'x': [1, 2, 3], 'y': [4, 5, 6]})
   return df 


@asset
def get_r_transformed_data(get_source_data):
    # Send source data to the Arrow Flight server
    df_arrow = pyarrow.Table.from_pandas(get_source_data)
    client = pa.flight.connect("grpc://0.0.0.0:8089")
    upload_descriptor = pa.flight.FlightDescriptor.for_path("test_data/my_df")
    writer, _ = client.do_put(upload_descriptor, df_arrow.schema)
    writer.write_table(df_arrow)
    writer.close()

    # Run R script which will be grabbing source data from the Arrow Flight server
    subprocess.check_output(["Rscript", "task.R"])

    # Get arrow output from the Arrow Flight server, that R process dropped
    upload_descriptor = pa.flight.FlightDescriptor.for_path("test_data/my_df2")
    flight = client.get_flight_info(upload_descriptor)
    descriptor = flight.descriptor
    reader = client.do_get(flight.endpoints[0].ticket)
    read_table = reader.read_all()
    my_df2 = read_table.to_pandas()
    return Output(
        value = my_df2,
        metadata={
            "num_records": len(my_df2),
            "preview": MetadataValue.md(my_df2.head().to_markdown()),
        }
    )
