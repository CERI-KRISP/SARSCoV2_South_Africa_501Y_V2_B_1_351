import pandas as pd
from collections import Counter

df=pd.read_excel('dataset_v3/501Y.V2_nextclade_n344.xlsx')

print(df)

mutations_df=df['aaSubstitutions'].dropna().apply(lambda x: x.rsplit(',')).tolist()
print(mutations_df)
flat_list=[]
for sublist in mutations_df:
    for item in sublist:
        flat_list.append(item)

print(flat_list)

S_mutations = [idx for idx in flat_list if idx[0] == 'S']

# print result
print("The list of matching first letter : " + str(S_mutations))

unique_S_mutations = []
for x in S_mutations:
    if x not in unique_S_mutations:
        unique_S_mutations.append(x)
print(unique_S_mutations)

print(Counter(S_mutations))

S_mutations_df=pd.DataFrame.from_dict(dict(Counter(S_mutations)), orient='index', columns=['frequency'])
print(S_mutations_df)

S_mutations_df.to_csv('501Y.V2_n344_spike_mutations.csv')
