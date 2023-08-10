clear;%\pshome\cmfo500\Desktop
clc;
%% Seed random number generator:
rng('shuffle');
Seed = floor(rand(1,1)*99999);
rng(Seed);
Mapping = GetMappings(Seed);
nSteps = 1000;

%% Set Labels:
L = [floor(65:(1/4):68.9)',mod((0:1:15)',4)+65];
StateLabels = cellfun(@(s)char(s),num2cell(L));

%% Set Matrix:
TransMat = [...
0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.00	0.09	0.81	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.09	0.00	0.01	0.00
0.00	0.00	0.00	0.00	0.00	0.00	0.01	0.09	0.00	0.00	0.00	0.00	0.09	0.81	0.00	0.00
0.00	0.00	0.00	0.00	0.01	0.00	0.09	0.00	0.09	0.81	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.01	0.00	0.09	0.00	0.09	0.81	0.00
0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.81	0.00	0.09	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.01	0.09	0.00	0.00
0.00	0.09	0.81	0.00	0.00	0.00	0.00	0.00	0.01	0.09	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.00	0.00	0.00	0.81	0.00	0.00	0.09	0.00	0.00	0.00	0.00	0.00	0.01	0.09	0.00
0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.09	0.00	0.00	0.01	0.81	0.00	0.09	0.00
0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.01	0.09	0.00	0.09	0.00	0.81	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.00	0.00	0.00	0.09	0.00	0.00	0.01	0.00	0.09	0.00	0.81	0.00	0.00	0.00	0.00
0.00	0.00	0.01	0.09	0.00	0.00	0.00	0.00	0.81	0.00	0.00	0.09	0.00	0.00	0.00	0.00
0.00	0.09	0.00	0.01	0.00	0.00	0.09	0.81	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00
0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.00

];

%% First Order transform:
Order1Mata = kron(eye(4),ones(4));
Order1Matb = kron(ones(4),eye(4));

%% Set inital state:
S1 = floor(rand(1,1)*size(StateLabels,1)) + 1;

%% Loop:
nStates = nSteps / 2;
Sequence = cell(nStates*2,1);
ProbsO1 = nan(nStates*2,1);
ProbsO2 = nan(nStates*2,1);
for iState = 1:1:nStates
    iInput1 = ((iState-1)*2)+1;
    iInput2 = ((iState-1)*2)+2;
    if iState == 1
        Sequence{iInput1} = StateLabels(S1,1);
        Sequence{iInput2} = StateLabels(S1,2);
        iPrevState = S1;
    else
        V = zeros(1,size(StateLabels,1));
        V(iPrevState) = 1;
        P = V*TransMat;
        CP = cumsum(P);
        R = rand(1,1);
        for ii = 1:1:numel(CP)
            if CP(ii) >= R
                break
            end
        end
        Sequence{iInput1} = StateLabels(ii,1);
        Sequence{iInput2} = StateLabels(ii,2);
        O1a = P*Order1Mata;
        ProbsO1(iInput1) = O1a(ii);
        O1b = P*Order1Matb;
        ProbsO1(iInput2) = O1b(ii);
        ProbsO2(iInput1) = P(ii);
        %%%
        iPrevState = ii;
    end
end

%%
SeqStruct = struct('StateId',{{}},'StateIndex',NaN,'KeyId',{{}},'ProbsO1',{NaN},'ProbsO2',{NaN});
for iSubState = 1:1:(nStates*2)
    SeqStruct(iSubState,1).StateId = Sequence{iSubState};
    SeqStruct(iSubState,1).StateIndex = double(Sequence{iSubState})-64;
    SeqStruct(iSubState,1).KeyId = Mapping{SeqStruct(iSubState,1).StateIndex};
    SeqStruct(iSubState,1).ProbsO1 = ProbsO1(iSubState);
    SeqStruct(iSubState,1).ProbsO2 = ProbsO2(iSubState);
end
ToWrite = [{SeqStruct.KeyId}',num2cell([[SeqStruct.ProbsO1]',[SeqStruct.ProbsO2]'])];
%ToWrite = [{'KeyId','ProbsO1','ProbsO2'};ToWrite];
% csvwrite(sprintf('MarkovChain_Seed-%i.csv',Seed),...
%     ToWrite);
Table = cell2table(ToWrite,'VariableNames',{'KeyId','ProbsO1','ProbsO2'});
% Write the table to a CSV file
writetable(Table,sprintf('MarkovChain_Seed_SRT3-%i.csv',Seed));