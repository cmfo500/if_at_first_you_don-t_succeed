This folder includes the code and data for the supplementary experiment presented in the manuscript -
Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again by Cátia M. Oliveira
Marianna E. Hayiou-Thomas, and Lisa. M. Henderson at the Department of Psychology, University of York.

1. There are three scripts:
- Modelling: includes all models presented in the manuscript
- Reliability: includes all reliability and individual differences analyses;

2. Three datasets:
	2.1 Data_SuppExp.csv includes all data required to run the scripts with the exception of performance on explicit awareness and attention tasks.
	This dataset includes the following variables:

	- Participant: Participant anonymised identifier;
	- Participant characteristics (Age, Gender, Nationality and Language (Monolingual, Bilingual, etc))
	- Procedural learning task variables:
		Epoch: Epoch number 1-5 (each epoch includes 200 trials);
		Trial: Trial number 1 - 1000;
		KeyId: There were four positions of the stimuli on screen ZXNM from left to right on the keyboard;
		Probability: Indicates the type of trial (1: Improbable, 2: Probable);
		Response: Indicates the key on the keyboard pressed by the participant;
		Accuracy: Indicates whether participants made the correct response (0 - incorrect, 1 - correct);
		RT: Time elapsed between stimulus presentation and motor response;
		Session: Indicates whether testing occurred in the first or second session;
		task: Combines the information about the group (ISI or noISI) and the sequence version;
		date: Time of completion of the SRT task;
	- Explicit awareness variables:
		Exp_Q1: Indicates whether participants were aware of a pattern in the task (y - yes, n - no, u - unsure);
		Exp_Q2: Indicates when participants became aware (1 - first session, 2 - second session);
	
	2.2 Explicit_awareness_data_SuppExp.csv includes the remaining explicit awareness measures based on the generation task for both inclusion and exclusion conditions.
	Variables
	- Participant: Participant anonymised identifier;
	- Triplet_inc: Total number of triplets in common between the sequence generated in the inclusion condition by the participant and the underlying sequence in the SRT task;
	- Diff_triplets_inc: Total number of distinct triplets in common between the sequence generated in the inclusion condition by the participant and the underlying sequence in the SRT task;
	- Longest_inc: Longest sequence generated in the inclusion condition  by the participant in common with the underlying sequence in the SRT task;
	- Triplet_exc: Total number of triplets in common between the sequence generated in the exclusion condition by the participant and the underlying sequence in the SRT task;
	- Diff_triplets_exc: Total number of distinct triplets in common between the sequence generated in the exclusion condition by the participant and the underlying sequence in the SRT task;
	- Longest_exc: Longest sequence generated in the exclusion condition by the participant in common with the underlying sequence in the SRT task;
	- Group: Indicates group membership (ISI or noISI);

	2.3 PVT_results_SuppExp.csv includes all performance details for the Psychomotor Vigilance task.
	Variables
	- Participant: Participant anonymised identifier;
	- Interval_stimulus: Indicates the duration of the interval between response and stimulus presentation;
	- RT: Time elapsed between stimulus presentation and motor response;
	- Session: Indicates whether testing occurred in the first or second session;
	- date: Time of completion of the Psychomotor vigilance task;
	- key_resp.keys: Indicates how many times participants pressed "space" on the keyboard before stimuli appearance often referred to as "false starts" (each press
	  is represented by one ["space"])
	- key_resp.rt: Indicates when the false starts occured before a new stimulus presentation;

3. There is also a result section where we included the output from the mixed modelling as these models take a fairly long time to run.
For the influential cases, since we had to resort to the High performance computing at York, we also included the output from those analyses.
We have run the same models in Julia and these ran a lot quicker, so we can provide that code if necessary.

If any information of relevance is missing please forward any requests to cmfo500@york.ac.uk