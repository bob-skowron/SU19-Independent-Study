
import wrds
import pandas as pd
import os


os.getcwd()
# list tables in the crsp dataset
db.list_tables(library="compd")

# get values from a single table
db.get_table(library="compd",table="idxcst_his",obs=10)
print(db.get_table(library="compd",table="co_hgic",obs=10).to_string())
print(db.get_table(library="compd",table="r_giccd",obs=10).to_string())

# select top 10 records from SP500 daily table to test connection
db.raw_sql("select caldt, spindx, sprtrn, totval from crsp.dsp500 limit 10")
db.raw_sql("select caldt, spindx, sprtrn, totval from crsp.dsp500 where caldt >= '1989-01-01' limit 10")

db.raw_sql("select *  from compd.secd where gvkey='001487' and datadate='2018-01-31'")
db.raw_sql("select *  from compd.idxcst_his h where gvkey='001487' and h.from <='2018-01-31' and case when h.thru is null then '9999-12-31' else h.thru end >= '2018-01-31'")

db.raw_sql(f"""select s.datadate, n.gvkeyx, s.gvkey, n.conm, s.tic, s.prccd as close, s.cshoc*s.prccd as mktcap, 
        rgsector.gicdesc as gsector, rggroup.gicdesc as ggroup, rgind.gicdesc as gind, rgsubind.gicdesc as gsubind
    from compd.names_ix n 
    join compd.idxcst_his c 
    on n.gvkeyx = c.gvkeyx
    join compd.secd s
    on c.gvkey = s.gvkey
    and s.iid = c.iid
    and s.datadate >= c.from
    and s.datadate <= case when c.thru is null then '9999-12-31' else c.thru end

    join compd.co_hgic g
    on s.gvkey = g.gvkey
    and s.datadate >= g.indfrom
    and s.datadate <= case when g.indthru is null then '9999-12-31' else g.indthru end

    join compd.r_giccd rgsector
    on g.gsector = rgsector.giccd and rgsector.gictype = 'GSECTOR'

    join compd.r_giccd rggroup
    on g.ggroup = rggroup.giccd and rggroup.gictype = 'GGROUP'

    join compd.r_giccd rgind
    on g.gind = rgind.giccd and rgind.gictype = 'GIND'

    join compd.r_giccd rgsubind
    on g.gsubind = rgsubind.giccd and rgsubind.gictype = 'GSUBIND'

    where n.gvkeyx='000003' 
    and s.datadate = '2018-01-31' LIMIT 550""")


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

def get_idx_return_data(db, startdate, enddate, idx_list):
    data = db.raw_sql(f"""select d.datadate, n.gvkeyx, n.conm, d.prccd as close, d.prccddiv as totalreturn 
    from compd.names_ix n join compd.idx_daily d 
    on n.gvkeyx = d.gvkeyx 
    where n.gvkeyx in ('{"','".join(idx_list)}') 
    and datadate >= '{startdate}' and datadate <= '{enddate}'""")
    return data

def get_idx_const_return_data(db, startdate, enddate, idx_list):
    db.raw_sql(f"""select s.datadate, n.gvkeyx, s.gvkey, n.conm, s.tic, s.prccd as close, s.cshoc*s.prccd as mktcap, 
            rgsector.gicdesc as gsector, rggroup.gicdesc as ggroup, rgind.gicdesc as gind, rgsubind.gicdesc as gsubind
        from compd.names_ix n 
        join compd.idxcst_his c 
        on n.gvkeyx = c.gvkeyx
        join compd.secd s
        on c.gvkey = s.gvkey
        and s.iid = c.iid
        and s.datadate >= c.from
        and s.datadate <= case when c.thru is null then '9999-12-31' else c.thru end

        join compd.co_hgic g
        on s.gvkey = g.gvkey
        and s.datadate >= g.indfrom
        and s.datadate <= case when g.indthru is null then '9999-12-31' else g.indthru end

        join compd.r_giccd rgsector
        on g.gsector = rgsector.giccd and rgsector.gictype = 'GSECTOR'

        join compd.r_giccd rggroup
        on g.ggroup = rggroup.giccd and rggroup.gictype = 'GGROUP'

        join compd.r_giccd rgind
        on g.gind = rgind.giccd and rgind.gictype = 'GIND'

        join compd.r_giccd rgsubind
        on g.gsubind = rgsubind.giccd and rgsubind.gictype = 'GSUBIND'

        where n.gvkeyx in ('{"','".join(idx_list)}') 
        and s.datadate >= '{startdate}' and s.datadate <= '{enddate}'""")
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

    # get more index data from compustat
    # S&P500, Russell 2000, Russell 3000, Wilshire 5000, S&P 1000 (midcap), S&P600 (smallcap), S&P 100 (large cap)
    idx_list = ["000003", "156765", "156931", "182779", "146884", "030824", "000664"]
    more_idx_data = get_idx_return_data(db, startdate, enddate, idx_list)
    more_idx_const = get_idx_const_return_data(db, startdate, enddate, idx_list)

    # close the connection
    db.close()