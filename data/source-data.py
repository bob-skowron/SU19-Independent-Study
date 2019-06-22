
import wrds
import pandas as pd
import os



# list tables in the crsp dataset
db.list_tables(library="crsp")

# select top 10 records from SP500 daily table to test connection
db.raw_sql("select caldt, spindx, sprtrn, totval from crsp.dsp500 limit 10")
db.raw_sql("select caldt, spindx, sprtrn, totval from crsp.dsp500 where caldt >= '1989-01-01' limit 10")

db.raw_sql("select a.*, b.ret, b.date from crsp.dsp500list as a join crsp.dsf as b on a.permno=b.permno and b.date >= a.start and b.date<= a.ending where b.date = '2018-12-31' order by date LIMIT 500;")

db.get_table(library="crsp",table="SICCD",obs=10)



def wrds_connect():
    # set up connection to WRDS
    db = wrds.Connection(wrds_username="rskowron")
    # db.create_pgpass_file() # create pgpass file to no longer require pwd
    return db 

def get_sp500_idx_return_data(db, startdate, enddate):
    """retrieves return information for the S&P500 index between the specified dates. Uses CRSP"""
    data = db.raw_sql(f"select caldt, spindx, sprtrn, totval from crsp.dsp500 where caldt >= '{startdate}' and caldt <= '{enddate}'")
    return data 

def get_sp500_const_return_data(db, startdate, enddate):
    """retrieves return information for S&P500 constituents between the specified dates. Uses CRSP"""
    data = db.raw_sql(f"""select s.date, sn.ticker, s.shrout, s.prc, s.ret, sn.siccd, abs(s.prc)*s.shrout as mtkcap
    from crsp.dsp500list as l join crsp.dsf as s 
    on l.permno=s.permno and s.date >= l.start and s.date <= l.ending
    join crsp.stocknames sn
    on s.permno = sn.permno and sn.namedt <= s.date and sn.nameenddt >= s.date
    where s.date >= '{startdate}' and s.date <= '{enddate}'
    order by s.date, sn.ticker""")
    return data 


if __name__ == "__main__":
    
    # connect to wrds
    db = wrds_connect()
    startdate = "2018-01-01"
    enddate = "2018-01-31"

    # get S&P500 data and write to file
    idx_data = get_sp500_idx_return_data(db, startdate, enddate)

    # get const data and write to file
    const_data = get_sp500_const_return_data(db, startdate, enddate)


    # close the connection
    db.close()