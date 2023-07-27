This folder includes the code and data for the supplementary experiment presented in the manuscript -
Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again by Cátia M. Oliveira
Marianna E. Hayiou-Thomas, and Lisa. M. Henderson at the Department of Psychology, University of York.

1. There are three scripts:
- Modelling: includes all models presented in the manuscript
- Reliability: includes all reliability and individual differences analyses;

2. Three datasets:
	2.1 Data.trimmed_study1.csv includes all data required to run the script KLD_testing1
	This dataset includes the following variables:

	- Participant: Participant anonymised identifier;
	- Procedural learning task variables:
		Block: Block number 1-20 (each block includes 50 trials);
		Epoch: Epoch number 1-5 (each epoch includes 200 trials);
		Trial_number: Trial number 1 - 1000;
		Probability: Indicates the type of trial (Improb: Improbable, Prob: Probable);
		Accuracy: Indicates whether participants made the correct response (0 - incorrect, 1 - correct);
		RT: Time elapsed between stimulus presentation and motor response;
		logRT: Log transformation of RT;
		Session: Indicates whether testing occurred in the first or second session;
	
	2.2 Study2_trimmed.csv includes all data required to run the script KLD_testing2.1 and KLD_testing2.2
	Variables
	- Participant: Participant anonymised identifier;
	- Procedural learning task variables:
		Epoch: Epoch number 1-5 (each epoch includes 200 trials);
		Trial_number: Trial number 1 - 1000;
		Probability: Indicates the type of trial (Improb: Improbable, Prob: Probable);
		Accuracy: Indicates whether participants made the correct response (0 - incorrect, 1 - correct);
		RT: Time elapsed between stimulus presentation and motor response;
		Session: Indicates whether testing occurred in the first or second session;

	2.3 SuppExp_trimmed includes all data required to run the script KLD_testing3
	Variables
	- Participant: Participant anonymised identifier;
	- Procedural learning task variables:
		Epoch: Epoch number 1-5 (each epoch includes 200 trials);
		Trial: Trial number 1 - 1000;
		KeyId: There were four positions of the stimuli on screen ZXNM from left to right on the keyboard;
		Probability: Indicates the type of trial (1: Improbable, 2: Probable);
		Accuracy: Indicates whether participants made the correct response (0 - incorrect, 1 - correct);
		RT: Time elapsed between stimulus presentation and motor response;
		Session: Indicates whether testing occurred in the first or second session;
		task: Combines the information about the group (ISI or noISI) and the sequence version;


If any information of relevance is missing please forward any requests to cmfo500@york.ac.uk