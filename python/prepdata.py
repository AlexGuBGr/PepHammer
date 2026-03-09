import glob
import pickle
import sqlite3
import pandas as pd
import numpy as np
#import networkx as nx

# peptipedia/ is a folder with all bioactive peptides divided into FASTE files. 
# The names of the FASTA files reveal the bioactivty of the peptides
dirr = "../../../peptipedia/"
files = glob.glob1(dirr, "*")
bionames = np.array([i.split(".")[0] for i in files])

vocab = {'D','Q','S','A','P','R','C','E','F','G','H','I','K','L','M','N','T','V','W','Y','X','B','J','Z','O','U'}


def get_peptides(dirr, files, vocab):
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
                if len(set(tmp[1]).difference(vocab)) == 0:
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

    print("Saving as Pickle files...")
    with open("bioactivty_dct.pickle", "wb") as f:
        pickle.dump(out, f)
    print("'bioactivty_dct.pickle' has been saved!")

    with open("notin_bioactivty_dct.pickle", "wb") as f:
        pickle.dump(notin, f)
    print("'notin_bioactivty_dct.pickle' has been saved!")

    return out

def sort_by_len(out, vocab, files_):
    print("Sorting peptides by lengths...")
    vocab_array = np.array(sorted(list(vocab)))
    #voze = np.zeros((len(vocab_array)))
    bioze = np.zeros((len(files_)))
    out_len = {}
    for k,v in out.items():
        k_len = str(len(k))
        #print(k_len)
        if k_len not in out_len:
            out_len[k_len] = {}
            out_len[k_len]["peptide"] = [k]

            biof = []
            ids = set()
            for i in v:
                biof.append(i[0])
                ids.add(i[1])
            out_len[k_len]["peptipedia_id"] = [list(ids)[0]]
            tmp1 = bioze.copy()
            tmp1[np.isin(files_, biof)] = 1
            out_len[k_len]["biofunctions"] = [[tmp1 ]]

            #tmp = voze.copy()
            #u,c = np.unique( list(k), return_counts=True )
            #tmp[np.isin(vocab_array,u)] = c
            #out_len[k_len]["aa"] = [[tmp]]
        else:
            out_len[k_len]["peptide"].append( k )

            biof = []
            ids = set()
            for i in v:
                biof.append(i[0])
                ids.add(i[1])
            out_len[k_len]["peptipedia_id"].append( list(ids)[0])
            tmp1 = bioze.copy()
            tmp1[np.isin(files_, biof)] = 1
            out_len[k_len]["biofunctions"].append([tmp1]) 

            #tmp = voze.copy()
            #u,c = np.unique( list(k), return_counts=True )
            #tmp[np.isin(vocab_array,u)] = c
            #out_len[k_len]["aa"].append( [tmp] )

    print("Saving as Pickle files...")
    with open("len_bioactivty_dct.pickle", "wb") as f:
        pickle.dump(out_len, f)
    print("'len_bioactivty_dct.pickle' has been saved!")

    return out_len


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
        

def convert_to_sql(out_len, vocab, bionames):

    vocab_list = sorted(list(vocab))    

    for k,v in out_len.items():
        intt = int(k)
        if intt > 1 and intt < 151:
            print(k)
            biofunctions = v.pop("biofunctions")
            #aa = v.pop("aa")
            df = pd.concat([pd.DataFrame(v), 
                            #pd.DataFrame(np.concatenate(aa,axis=0), columns=vocab_list), 
                            pd.DataFrame(np.concatenate(biofunctions,axis=0), columns=bionames)], axis=1)
            save_table_to_db(df, "../db/biofuncs.sqlite", k, indx=False)



if __name__ == "__main__":
    convert_to_sql( sort_by_len( get_peptides(dirr, files, vocab), vocab, bionames), vocab, bionames )