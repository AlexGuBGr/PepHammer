#import glob
#import pickle
import sqlite3
import pandas as pd
import numpy as np


def get_table_from_db(dbname, table):
    con = sqlite3.connect(dbname, timeout = 10)
    try:
        data = pd.read_sql("SELECT * FROM '{}'".format(table), con)
    except:
        print("did not get data -- ERROR")
        data = None
    finally:
        con.close()
    return data

def get_table_names(dbname):
    conn = sqlite3.connect(dbname)
    cur = conn.cursor()
    try:
        cur.execute("SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%';")
        data = cur.fetchall()
        data = [row[0] for row in data]
    except:
        print("did not get data -- ERROR")
        data = None
    finally:
        cur.close()
        conn.close()
    return data


def save_table_to_db(df, dbname, table, indx=False):
    con = sqlite3.connect(dbname, timeout = 10)#, isolation_level = None)
    try:
        df.to_sql(name=table, con=con, index=indx, if_exists="replace")
    except:
        print("could not save...")
    finally:
        con.close()

def vacuum_db(dbname): 

    conn = sqlite3.connect(dbname)
    cur = conn.cursor()
    try:
        cur.execute("VACUUM")
    except:
        print("could not vacuum...")
    finally:
        cur.close()
        conn.close()

def get_columns_from_db(dbname, table, columns):
    cols = ",".join(["`{}`".format(i) for i in columns])
    con = sqlite3.connect(dbname, timeout = 10)

    try:
        data = pd.read_sql("SELECT {} FROM '{}'".format(cols, table), con)
    except:
        print("did not get data -- ERROR")
        data = None
    finally:
        con.close()
    return data

def delete_rows_based_on_column_items(dbname, table, column, items):

    if len(items) == 0:
        return  

    placeholders = ",".join(["?"] * len(items))
    query = f"DELETE FROM `{table}` WHERE `{column}` IN ({placeholders})"

    print(query)

    with sqlite3.connect(dbname) as conn:
        cursor = conn.cursor()
        cursor.execute(query, items)
        conn.commit()


def prune_peps(dbname, prcnt=0.2):
    
    alltables = get_table_names(dbname) 
     
    for i in alltables:
        tmptable = get_columns_from_db(dbname,i,["peptide"])
        n = []
        for ii in tmptable.peptide:
            tmp = ii.count("X") / float(i)
            if tmp > prcnt:
                n.append(ii)
        print(i, " - deleting", len(n), "rows" ) 
        if len(n) > 0:
            delete_rows_based_on_column_items(dbname, i, "peptide", n)

    vacuum_db(dbname)


dbname = "../db/biofuncs.sqlite"
if __name__ == "__main__":
    prune_peps(dbname, prcnt=0.2)