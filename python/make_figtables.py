import sqlite3
import pandas as pd
import numpy as np



familynames = ['7B2', 'ACBP', 'AKH/HRTH/RPCH', 'AVIT (prokineticin)', 'Adrenomedullin', 'Allatostatin', 'Allatotropin', 'Apelin', 'Arthropod CHH/MIH/GIH/VIH hormone', 'Arthropod PDH', 'Augurin', 'Bombesin/neuromedin-B/ranatensin', 'Bradykinin-related peptide', 'Buccalin', 'Bursicon', 'CART', 'CCAP', 'CCHamide', 'Calcitonin', 'Cerebellins', 'Chromogranin/secretogranin', 'Corazonin', 'Dermorphin', 'Diuretic hormone class 2', 'Ecdysis triggering hormone', 'Endothelin/sarafotoxin', 'FMRFamide related peptide', 'Fliktin/Flik', 'Galanin', 'Gastrin/cholecystokinin', 'Glucagon', 'GnRH', 'HIRamide', 'Insect eclosion hormone', 'Insulin', 'KISS1', 'Kinin', 'LWamide neuropeptide', 'Leptin', 'Melanin-concentrating hormone', 'Molluscan ELH', 'Motilin', 'Myomodulin', 'Myosuppressin', 'NPY', 'NVP-like peptides', 'Natalisin', 'Natriuretic peptide', 'Neurexophilin', 'Neuromedins', 'Neuropeptide B/W', 'Neuropeptide-like peptide', 'Neurotensin', 'Nucleobindin', 'Opioid', 'Orcokinin', 'Orexin', 'POMC', 'Parathyroid hormone', 'Pedal peptide', 'Periviscerokinin', 'ProSAAS', 'Proctolin', 'Pyrokinin', 'QWamide', 'RFamide neuropeptide', 'Resistin/FIZZ', 'SCP', 'Sauvagine/corticotropin-releasing factor/urotensin I', 'Serpin', 'Somatostatin', 'Somatotropin/prolactin', 'Spexin', 'TRH', 'Tachykinin', 'Tenascin', 'Urotensin-2', 'VGF', 'Vasopressin/oxytocin', 'YGGW-amide related peptide']



allbionames_pep = ['Activator','Allergen','Amylase inhibitor','Angiotensin-converting enzyme (ace) inhibitors',
				'Angiotensinase inhibitor','Anti adenoviridae','Anti adenovirus','Anti african swine fever virus','Anti aging',
				'Anti allergen','Anti alloherpesviridae','Anti amnesic','Anti andes virus','Anti angiogenic','Anti arenaviridae',
				'Anti arteriviridae','Anti asfarviridae','Anti avian influenza virus','Anti avian myeloblastosis virus',
				'Anti avian sarcoma and leukosis virus- a','Anti biofilm','Anti bk virus','Anti bovine herpesvirus type 1',
				'Anti bovine rotavirus','Anti bunyaviridae','Anti caliciviridae','Anti candida','Anti canine coronavirus',
				'Anti channel catfish virus','Anti chikungunya virus','Anti classical swine fever virus','Anti coronaviridae',
				'Anti coronavirus','Anti cowpox virus','Anti coxsackie virus','Anti dengue virus','Anti diabetic type 1',
				'Anti diabetic type 2','Anti diabetic','Anti duck hepatitis virus','Anti ebola virus','Anti encephalomyocarditis virus',
				'Anti endotoxin','Anti enterovirus','Anti epstein-barr virus','Anti feline coronavirus','Anti feline immunodeficiency virus',
				'Anti feline leukemia virus','Anti filoviridae','Anti flaviviridae','Anti foot-and-mouth disease virus',
				'Anti frog virus','Anti fungal','Anti gram (+)','Anti gram (-)','Anti grass carp hemorrhage virus','Anti H1N1 virus',
				'Anti H3N2 virus','Anti H5N1 virus','Anti hantaviridae','Anti hendra virus','Anti hepatitis B virus','Anti hepatitis C virus',
				'Anti hepatitis E virus','Anti herpes simplex virus','Anti herpesviridae','Anti HIV','Anti human coronavirus',
				'Anti human cytomegalovirus','Anti human metapneumovirus','Anti human papiloma virus','Anti human parainfluenza virus',
				'Anti human t-cell leukaemia virus 1','Anti human t-lymphotropic virus','Anti hypertensive','Anti hypotensive',
				'Anti inflamatory','Anti influenza virus','Anti iridoviridae','Anti japanese encephalitis virus','Anti junin virus',
				"Anti kaposi's sarcoma-associated herpes virus",'Anti lymphocytic choriomeningitis virus','Anti malarial',
				'Anti mammalian cell',"Anti marek's disease virus",'Anti measles virus','Anti MERS-COV','Anti metapneumovirus',
				'Anti methicillin-resistant s','Anti mink enteritis virus','Anti minute virus of mice','Anti mollicute',
				'Anti monkeypox virus','Anti mouse hepatitis virus','Anti murine hepatitis virus','Anti murine leukemia virus',
				'Anti murine norovirus','Anti nematode','Anti nervous necrosis virus','Anti newcastle disease virus','Anti nimaviridae',
				'Anti nipah virus','Anti nodaviridae','Anti orthoherpesviridae','Anti orthomyxoviridae','Anti oxidative',
				'Anti papillomaviridae','Anti paramyxoviridae','Anti parvoviridae','Anti phenuiviridae','Anti pichinde virus',
				'Anti picornaviridae','Anti polyomaviridae','Anti porcine reproductive and respiratory syndrome virus',
				'Anti porcine respiratory coronavirus','Anti poxviridae','Anti protist','Anti puumala virus','Anti rabies virus',
				'Anti reoviridae','Anti respiratory syncytial virus','Anti retroviridae','Anti rhabdoviridae','Anti rhinovirus',
				'Anti rift valley fever virus','Anti SARS-COV 2','Anti SARS-COV','Anti sedoreoviridae','Anti sendai virus',
				'Anti simian immunodeficiency virus','Anti simian rotavirus','Anti sin nombre virus','Anti singapore grouper iridovirus',
				'Anti sl-cov-wiv1','Anti tacaribe virus','Anti tembusu virus','Anti thrombotic','Anti togaviridae','Anti toxin',
				'Anti transmissible gastroenteritis virus','Anti tuberculosis','Anti vaccinia','Anti varicella zoster virus',
				'Anti vesicular stomatistic virus','Anti west nile virus','Anti white spot syndrome virus','Anti yellow fever virus',
				'Anti zika virus','Antiatherosclerotic','Antibacterial','Anticancer','Antimicrobial','Antiosteoporosis','Antiparasitic',
				'Antiprotozoal','Antiviral','Anuran defense','Apoptosis induction','Bacteriocin','Blood brain barrier penetrating',
				'Cell cell communication','Cell penetrating','Chemotactic','Coagulation + vascular','Cosmetic and dermatology',
				'Cytokine','Cytotoxic','DNA-binding','Drug delivery vehicle','Enzyme activator','Enzyme inhibitor','Glucose',
				'Gluten immunogenic + celiac toxic','Hemolytic','Hormone','Immunological','Immunomodulatory','Immunoregulator','Inhibitor',
				'Insecticidal','Metabolic','Molecular binding','Molluscicidal','Neurological','Neuropeptide','Neurotoxin',
				'Opioid-agonist','Opioid-antagonist','Opioid','Other','Pain related','Pheromone','Potentiator','Propeptide',
				'Protease inhibitor','Protein-protein interaction','Quorum sensing','Ralf-binding','Regulatory','Signal peptide',
				'Sperm activating','Spermicide','Stimulator','Surface immobilized','Taste','Therapeutic','Toxic','Transit',
				'Tumor homing','Tumor targeting','Venous','Wound healing']


mulpclass = ['hemolytic', 'toxic', 'antimicrobial', 'antivirus',
			   'antiparasite', 'anticancer', 'antibacterial', 'antifungal',
			   'cellcellsignaling', 'neuropeptide', 'peptidehormone', 'antifreeze',
			   'cytokines_growthfactors', 'antioxidative', 'drugdelivery', 'opioid',
			   'ACE inhibitor', 'antihypertensive', 'antidiabetes',
			   'dipeptidyl peptidase inhibitor']



tissues = ['ARC_BRAIN_PXD008795',
				'Ascending_colon_PXD009788', 'CSF_PXD062419', 'CTX_BRAIN_PXD008795',
				'CTX_neurons_ACN_PXD008795', 'CVaF_PXD004450', 'Duodenum_PXD009788',
				'Ileum_PXD009788', 'Jejenum_Post_gastrectomy_PXD011498',
				'Jejenum_Pre-gastrectomy_PXD011498', 'Jejunum_PXD009788',
				'POMC_neurons_GuHCL_PXD008795', 'PVN_BRAIN_PXD008795',
				'Serum_MASLD_PXD052061', 'Stomach_PXD009788', 'Urine_T1D_PXD012210',
				'Urine_control_PXD012210', 'pancreatic_islets_PXD026095',
				'plasma_healthy_PXD003533', 'plasma_healthy_PXD008141',
				'plasma_healthy_exercise_PXD007191', 'rectum_PXD009788',
				'serum_healthy_PXD008141', 'sigmoid_colon_PXD009788']



def make_legnth_vs_bioact(dbpath, table, nms, tiss = [], strr = False, thrs = [0.5]):
    print(dbpath, table)
    df = get_table_from_db(dbpath, table)
    Pep_length = np.sort(np.unique(df.Length))
    Pep_length = Pep_length[(Pep_length > 1 ) * (Pep_length < 151)]
    nmsnump = np.array(nms)
    addd = ""
    if strr == True:

        if len(tiss) > 0:
            addd = "_tissue"
            df1 = pd.concat([ df[df.Length == i][tiss].sum(0) for i in Pep_length], axis=1)
            df1 = df1.rename(columns={j:i for j,i in enumerate(Pep_length) })
            df1 = df1.T.reset_index().rename(columns = {"index": "Pep_length"})

        ze = np.zeros((len(Pep_length), len(nms)))
        for i in df[["Length", "bioactivities"]].to_numpy():
            ze[np.where(Pep_length == int(i[0]))[0][0]][ np.isin(nmsnump, i[1].split(";") ) ] += 1
        df = pd.DataFrame(ze, columns=nms)
        df["Pep_length"] = Pep_length


        if len(tiss) > 0:
            df = df.merge(df1, on="Pep_length")

        save_table_to_db(df, dbpath, 'length_bio_dist' + addd)
        #vacuum_db(dbpath)

    else:
        dfs = []
        if len(tiss) > 0:
            addd = "_tissue"
        nms = nms + tiss
        for thr in thrs:
            df1 = pd.concat([ (df[df.Length == i][nms] > thr).sum(0) for i in Pep_length], axis=1)
            df1 = df1.rename(columns={j:i for j,i in enumerate(Pep_length) })
            df1 = df1.T.reset_index().rename(columns = {"index": "Pep_length"})
            #if len(thrs) > 1:
            df1["pred"] = ">"+str(thr)
            dfs.append(df1)
        dfs = pd.concat(dfs, axis=0)
        save_table_to_db(dfs, dbpath, 'length_bio_dist' + addd)
        #vacuum_db(dbpath)


def make_bio_across_peptipedia(dbname, table, bioacts):
    print(dbname, table)
    dct = {}
    df = get_table_from_db(dbname, table)
    for i in bioacts:
        #df = get_columns_from_db_where_likecat(dbname, table, ["peptide", "bioactivities"], "bioactivities", i)
        ze = np.zeros( (len(bioacts)) )
        for jj,ii in enumerate(df.bioactivities):
            if i in ii:
                ze[[bioacts.index(iii) for iii in ii.split(";")]] += 1
        dct[i] = ze
    dct["rownames"] = bioacts
    save_table_to_db(pd.DataFrame(dct), dbname, 'bioact_vs_bioact')   
    #return pd.DataFrame(dct)


def make_bio_across_peptipedia_tissue(dbname, table, bioacts, tissues):
    print(dbname, table)
    dct = {}
    df = get_table_from_db(dbname, table)
    ze = np.zeros( (len(df), len(bioacts)) )
    for jj,ii in enumerate(df.bioactivities):
        ze[jj][[bioacts.index(iii) for iii in ii.split(";")]] += 1
    
    df = np.concatenate([ze, df[tissues] ], axis=1)
    alll = bioacts + tissues
    for i in alll:
        dct[i] = np.sum( df[ df[:, alll.index(i)] == 1 ], axis=0)
    dct["rownames"] = alll
    #return pd.DataFrame(dct)
    save_table_to_db(pd.DataFrame(dct), dbname, 'bioact_vs_bioact'+"_tissue")



def make_bio_across_matrix(dbname, table, all_bioacts, thrs = [0.5, 0.7, 0.9], tiss = ""):
    print(dbname, table)
    df = get_table_from_db(dbname, table)[all_bioacts]
    tmp = []
    for t in thrs:
        dct = {}
        for i in all_bioacts:
            dct[i] = (df[ df[i] > t ] > t).sum(axis=0).to_numpy()

        #if len(thrs) > 1:
        dct["pred"] = ">"+str(t)
        dct["rownames"] = all_bioacts
        tmp.append( pd.DataFrame(dct) )
    save_table_to_db(pd.concat(tmp, axis=0), dbname, 'bioact_vs_bioact'+tiss)
    #return pd.concat(tmp, axis=0)





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
        
        

if __name__ == "__main__":
    make_legnth_vs_bioact("../db/neuropepv2_one.sqlite", "neuropepv2", familynames, strr = False)
    make_legnth_vs_bioact("../db/peptipedia_bio_strings.sqlite", "peptipedia", allbionames_pep, strr = True)
    make_legnth_vs_bioact("../db/peptipedia_bio_strings.sqlite", "peptipedia_tissue", allbionames_pep, tissues, strr = True)

    make_legnth_vs_bioact("../db/pepti_multipep.sqlite", "pepti_multipep", mulpclass, strr = False, thrs = [0.5,0.7,0.9])
    make_legnth_vs_bioact("../db/multipep_tissue.sqlite", "multipep_tissue", mulpclass, tiss = tissues, strr = False, thrs = [0.5,0.7,0.9])


    make_bio_across_peptipedia_tissue("../db/peptipedia_bio_strings.sqlite", "peptipedia_tissue", allbionames_pep, tissues)
    make_bio_across_peptipedia("../db/peptipedia_bio_strings.sqlite", "peptipedia", allbionames_pep)

    make_bio_across_matrix("../db/pepti_multipep.sqlite", 'pepti_multipep', mulpclass)
    make_bio_across_matrix("../db/multipep_tissue.sqlite", 'multipep_tissue', mulpclass+ tissues, tiss = "_tissue")
    make_bio_across_matrix("../db/neuropepv2_one.sqlite", 'neuropepv2', familynames, thrs = [0.5])


    vacuum_db("../db/neuropepv2_one.sqlite")
    vacuum_db("../db/peptipedia_bio_strings.sqlite")
    vacuum_db("../db/pepti_multipep.sqlite")
    vacuum_db("../db/multipep_tissue.sqlite")