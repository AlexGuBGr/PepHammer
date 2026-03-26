import sqlite3
import pandas as pd
import numpy as np


vocab = {'D','Q','S','A','P','R','C','E','F','G','H','I','K','L','M','N','T','V','W','Y','X','B','J','Z','O','U'}


def save_table_to_db(df, DBname, table, indx=False):

    con = sqlite3.connect(DBname, timeout = 10)#, isolation_level = None)
    #con.isolation_level = None
    try:
        #c = con.cursor()
        #c.execute("begin")
        df.to_sql(name=table, con=con, index=indx, if_exists="replace")
        #c.execute("commit")
    except:
        print("could not save...")
        #c.execute("rollback")
    finally:
        con.close()


def createdb_index(DBname, table):
    try:
        conn = sqlite3.connect(DBname)
        cur = conn.cursor()
        query = f"CREATE INDEX IF NOT EXISTS idx_{table}_length ON {table}(Length);"
        print(query)
        cur.execute(query)
    except Exception as e:
        print(e)
    finally:
        cur.close()
        conn.close()


def check_index(DBname, table):

    try:
        conn = sqlite3.connect(DBname)
        cur = conn.cursor()
        sql = f"""
            EXPLAIN QUERY PLAN
            SELECT 'peptide' FROM {table} WHERE Length = 7;
            """
        cur.execute(sql)
        for row in cur.fetchall():
            print(row)

    except Exception as e:
        print(e)
    finally:
        cur.close()
        conn.close()


def set_to_wal(DBname):

    con = sqlite3.connect(DBname, timeout = 10)#, isolation_level = None)
    #con.isolation_level = None
    try:
        c = con.cursor()
        c.execute('pragma journal_mode=wal')
        wal_mode = c.fetchone()[0]
        print("DB mode: {}".format(wal_mode))
        
    except:
        print("could not set tp WAL mode")
    finally:
        c.close()
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


def prep_neuro(df, vocab):
    print(df.shape)
    
    boolrem = []
    for j,i in enumerate( list( df.Sequence )):
        if len(set(i).difference(vocab)) > 0:
            boolrem.append(False)
        else:
            boolrem.append(True)
    
    df = df[np.array(boolrem)]
    print(df.shape)
    
    df = df.rename(columns={"Sequence":"peptide"})
    
    df["Family"][df.Family.isin(['insect eclosion hormone'])] = 'Insect eclosion hormone'
    unique_fams = list(set(df.Family))
    unique_fams.remove(np.nan)
    df = df[df.Family.isin(unique_fams)]
    df = df[['NPID', 'peptide', 'Length', 'Name', 'Family', 'Organism']]

    agg_fun = lambda x: ";".join(np.unique(x.astype(str)))

    df = (
        df.groupby("peptide", as_index=False)
          .agg({c: agg_fun for c in df.columns if c != "peptide"})
    )

    df["Length"] = pd.to_numeric(df["Length"])


    unq = sorted(unique_fams)
    print(unq)
    dffams = df[["Family"]].copy()
    df = df[['peptide', 'Length', 'Name', 'NPID', 'Organism']].copy()
    
    ze = np.zeros((len(df), len(unq)))
    
    for j,i in enumerate(dffams.Family):
        tmp = i.split(";")
        ze[j][np.isin(unq, tmp)] = 1

    return pd.concat( [ df, pd.DataFrame(ze, columns=unq)], axis=1)



def add_to_db(df, dbnam):

    #Length not in: [  2,  96,  98, 105, 107, 109, 136, 142, 147, 148]
    
    lens = np.sort( df.Length )
    unq_lens = np.unique(lens)
    
    for i in unq_lens:
        if i > 150:
            continue
            
        dftmp = df[df.Length == i].copy()
        
        n = np.zeros((len(dftmp)))
        for jj,ii in enumerate( dftmp.peptide ):
            tmp = ii.count("X") / i
            if tmp > 0.2:
                n[jj] = 1
        ns = np.sum(n)
        
        if ns > 0:
            print("\n", i, ns)
            print(dftmp.shape)
            dftmp = dftmp[n == 0]
            print(dftmp.shape)

        save_table_to_db(dftmp, dbnam, str(i), indx=False)





if __name__ == "__main__":
    df = pd.read_csv("neuropeptide_excel_NeuroPepV2_neuropeptide_all.txt", sep="\t")
    df = prep_neuro(df, vocab)
    save_table_to_db(df, "../db/neuropepv2_one.sqlite", "neuropepv2")
    vacuum_db("../db/neuropepv2_one.sqlite")
    set_to_wal("../db/neuropepv2_one.sqlite")
    
    createdb_index("../db/neuropepv2_one.sqlite", 'neuropepv2')
    check_index("../db/neuropepv2_one.sqlite" , 'neuropepv2')
    
    #add_to_db(df, "neuropepv2.sqlite")
    #vacuum_db("neuropepv2.sqlite")