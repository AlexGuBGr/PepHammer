import glob
import pickle
import sqlite3
import pandas as pd
import numpy as np
#import networkx as nx

# peptipedia/ is a folder with all bioactive peptides divided into FASTA files. 
# The names of the FASTA files reveal the bioactivty of the peptides
dirr = "../../../peptipedia/"
files = glob.glob1(dirr, "*")
bionames = np.array([i.split(".")[0] for i in files])

vocab = {'D','Q','S','A','P','R','C','E','F','G','H','I','K','L','M','N','T','V','W','Y','X','B','J','Z','O','U'}


## DB functions
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
        
def vacuum_db(DBname): 
  
    conn = sqlite3.connect(DBname)
    
    cur = conn.cursor()
    
    try:
        cur.execute("VACUUM")
    except:
        print("could not vacuum...")
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

##############


def get_peptides(dirr, files, vocab, prcnt = 0.2):
    print("Iterating through Peptipedia files...")
    out = {}
    notin = set()
    for i in files:
        biofunction = i.split(".")[0]
        print(biofunction)
        with open(dirr + i, "r", encoding="utf-8") as f:
            lines = f.read().split(">")

        for ii in lines:
            tmp = ii.split("\n")
            if len(tmp) == 3:
                tmp[1] = tmp[1].upper()
                intt = len(tmp[1])
                if len(set(tmp[1]).difference(vocab)) == 0 and tmp[1].count("X") / intt <= prcnt and intt > 1 and intt < 151:
                    data = (biofunction, tmp[0])
                    if tmp[1] not in out:
                        out[tmp[1]] = {data}
                    else: 
                        if data not in out[tmp[1]]:
                            out[tmp[1]].update( [data] )
                else:
                    notin.add(ii)
            else:
                notin.add(ii)
                
    print("filtered away:", len(notin))
    
    print("Saving as Pickle files...")
    with open("bioactivty_dct.pickle", "wb") as f:
        pickle.dump(out, f)
    print("'bioactivty_dct.pickle' has been saved!")
    
    return out


def make_table(dct, cats, dbpath, tablename):
    cats = list(cats)
    ze = np.zeros((len(dct), len(cats))) 
    dctnames = sorted_list = sorted(list(dct.keys()), key=lambda x: (len(x), x))
    ids = []
    ln = []
    for j,i in enumerate(dctnames):
        for ii in dct[i]:
            ze[j][cats.index(ii[0])] = 1
        ids.append(ii[1])
        ln.append(len(i))
    ze = pd.DataFrame(ze, columns=cats)
    ze["peptipedia_id"] = ids
    ze["peptide"] = dctnames
    ze["Length"] = ln
    
    save_table_to_db(ze, dbpath, tablename)



def make_table_strings(dct, cats, dbpath, tablename):
    cats = list(cats)
    ze = np.zeros((len(dct), len(cats))) 
    dctnames = sorted_list = sorted(list(dct.keys()), key=lambda x: (len(x), x))
    ids = []
    ln = []
    bioacts = []
    for j,i in enumerate(dctnames):
        bioacts_tmp = set()
        for ii in dct[i]:
            bioacts_tmp.add(ii[0])
        ids.append(ii[1])
        ln.append(len(i))
        bioacts.append( ";".join(bioacts_tmp) )
        
    df = pd.DataFrame( {
        "bioactivities":bioacts,
        "ID":ids,
        "peptide": dctnames,
        "Length":ln
    } ).sort_values( by=["Length", "peptide"] ).reset_index(drop=True)

    save_table_to_db(df, dbpath, tablename)



if __name__ == "__main__":
    #convert_to_sql( sort_by_len( get_peptides(dirr, files, vocab), vocab, bionames), vocab, bionames )
    make_table_strings( get_peptides(dirr, files, vocab, prcnt = 0.2), bionames, "../db/peptipedia_bio_strings.sqlite", 'peptipedia')
    
    print("Vacuuming....")
    vacuum_db("../db/peptipedia_bio_strings.sqlite")
    
    set_to_wal("../db/peptipedia_bio_strings.sqlite")