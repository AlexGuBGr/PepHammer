library(shiny)
library(bslib)
library(RSQLite)
library(DBI)
library(plotly)
library(future)
library(promises)
library(future.callr)
#library(progressr)

workers <- 6
plan(callr, workers = workers)

#progressr::handlers(global = TRUE)
#progressr::handlers("shiny")
#options(future.conditions = "condition")


source("R/dbfunctions.R")
source("R/functions.R")

dbselector = list(	"Peptipedia" = "db/peptipedia_bio_strings.sqlite", 
					"Peptipedia|Tissue" = "db/peptipedia_bio_strings.sqlite",
					
					"MultiPep" = "db/pepti_multipep.sqlite", 
					"MultiPep|Tissue" = "db/multipep_tissue.sqlite",

					"NeuroPep_v2" = "db/neuropepv2_one.sqlite")
					
tableselector = list(	"Peptipedia" = "peptipedia", 
						"Peptipedia|Tissue" = "peptipedia_tissue",
						
						"MultiPep" = "pepti_multipep", 
						"MultiPep|Tissue" = "multipep_tissue",

						"NeuroPep_v2" = "neuropepv2")

#dbpath = "db/biofuncs.sqlite"
#set_to_WAL(dbpath = dbpath)

aa_no_OU <- c('D','Q','S','A','P','R','C','E','F','G','H','I','K','L','M','N','T','V','W','Y','X','B','J','Z')
aa_levels <- c('D','Q','S','A','P','R','C','E','F','G','H','I','K','L','M','N','T','V','W','Y','X','B','J','Z','O','U')#, '_')

compat <- build_compat_matrix(aa_levels)
#peplen <- load_columns_from_table("Pep_length", "length_bio_dist",  dbpath)
grantm <- extend_grantham( grantham_distance_matrix() )



if (Sys.info()[['user']] == 'shiny'){
  # Running on shinyapps.io
} else {
  options(shiny.error = browser)
  options(shiny.fullstacktrace = TRUE)
}


table_lengths <- list(
	pepti_multipep=1334299,
	multipep_tissue=78213,
	peptipedia=3923151,
	peptipedia_tissue=17824,
	neuropepv2=7153
	)


familynames <- c('7B2', 'ACBP', 'AKH/HRTH/RPCH', 'AVIT (prokineticin)', 'Adrenomedullin', 'Allatostatin', 'Allatotropin', 'Apelin', 'Arthropod CHH/MIH/GIH/VIH hormone', 'Arthropod PDH', 'Augurin', 'Bombesin/neuromedin-B/ranatensin', 'Bradykinin-related peptide', 'Buccalin', 'Bursicon', 'CART', 'CCAP', 'CCHamide', 'Calcitonin', 'Cerebellins', 'Chromogranin/secretogranin', 'Corazonin', 'Dermorphin', 'Diuretic hormone class 2', 'Ecdysis triggering hormone', 'Endothelin/sarafotoxin', 'FMRFamide related peptide', 'Fliktin/Flik', 'Galanin', 'Gastrin/cholecystokinin', 'Glucagon', 'GnRH', 'HIRamide', 'Insect eclosion hormone', 'Insulin', 'KISS1', 'Kinin', 'LWamide neuropeptide', 'Leptin', 'Melanin-concentrating hormone', 'Molluscan ELH', 'Motilin', 'Myomodulin', 'Myosuppressin', 'NPY', 'NVP-like peptides', 'Natalisin', 'Natriuretic peptide', 'Neurexophilin', 'Neuromedins', 'Neuropeptide B/W', 'Neuropeptide-like peptide', 'Neurotensin', 'Nucleobindin', 'Opioid', 'Orcokinin', 'Orexin', 'POMC', 'Parathyroid hormone', 'Pedal peptide', 'Periviscerokinin', 'ProSAAS', 'Proctolin', 'Pyrokinin', 'QWamide', 'RFamide neuropeptide', 'Resistin/FIZZ', 'SCP', 'Sauvagine/corticotropin-releasing factor/urotensin I', 'Serpin', 'Somatostatin', 'Somatotropin/prolactin', 'Spexin', 'TRH', 'Tachykinin', 'Tenascin', 'Urotensin-2', 'VGF', 'Vasopressin/oxytocin', 'YGGW-amide related peptide')



allbionames_pep <- c('Activator','Allergen','Amylase inhibitor','Angiotensin-converting enzyme (ace) inhibitors',
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
				'Tumor homing','Tumor targeting','Venous','Wound healing')
				
				
mulpclass <- sort(c('hemolytic', 'toxic', 'antimicrobial', 'antivirus',
			   'antiparasite', 'anticancer', 'antibacterial', 'antifungal',
			   'cellcellsignaling', 'neuropeptide', 'peptidehormone', 'antifreeze',
			   'cytokines_growthfactors', 'antioxidative', 'drugdelivery', 'opioid',
			   'ACE inhibitor', 'antihypertensive', 'antidiabetes',
			   'dipeptidyl peptidase inhibitor'))
			   
	   
	   
tissues <- sort(c('ARC_BRAIN_PXD008795',
				'Ascending_colon_PXD009788', 'CSF_PXD062419', 'CTX_BRAIN_PXD008795',
				'CTX_neurons_ACN_PXD008795', 'CVaF_PXD004450', 'Duodenum_PXD009788',
				'Ileum_PXD009788', 'Jejenum_Post_gastrectomy_PXD011498',
				'Jejenum_Pre-gastrectomy_PXD011498', 'Jejunum_PXD009788',
				'POMC_neurons_GuHCL_PXD008795', 'PVN_BRAIN_PXD008795',
				'Serum_MASLD_PXD052061', 'Stomach_PXD009788', 'Urine_T1D_PXD012210',
				'Urine_control_PXD012210', 'pancreatic_islets_PXD026095',
				'plasma_healthy_PXD003533', 'plasma_healthy_PXD008141',
				'plasma_healthy_exercise_PXD007191', 'rectum_PXD009788',
				'serum_healthy_PXD008141', 'sigmoid_colon_PXD009788'))
				
				
colnameselector = list(	"Peptipedia" = allbionames_pep, 
						"Peptipedia|Tissue" = c(allbionames_pep, tissues),
						
						"MultiPep" = mulpclass, 
						"MultiPep|Tissue" = c(mulpclass, tissues),

						"NeuroPep_v2" = familynames)