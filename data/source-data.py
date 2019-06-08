
import wrds
import pandas as pd
import os

# set up connection to WRDS
db = wrds.Connection(wrds_username="rskowron")
# db.create_pgpass_file() # create pgpass file to no longer require pwd 

db.close()