import pandas as pd
from collections import Counter
import numpy as np

df=pd.read_excel('501Y.V2_nextclade_n344.xlsx')

print(df)

df['spike_mutations']=df['aaSubstitutions'].dropna().apply(lambda x: x.rsplit(',')).apply(lambda x: [idx for idx in x if idx[0] == 'S'])
df=df[['seqName','substitutions','totalMutations','deletions','spike_mutations','aaSubstitutions','totalAminoacidSubstitutions','aaDeletions']]
print(df)

def spike_mutations_num(a):
    try:
        count=len(a)
    except:
        count=0
    return count


df['Num_spike_mutations']=df['spike_mutations'].apply(lambda x: spike_mutations_num(x))
print(df)

def determine_positions(mutations_list):
    #print(mutations_list)
    positions_list=[]
    try:
        for x in mutations_list:
            #print(x)
            pos=int(x[3:-1])
            #print(pos)
            positions_list.append(pos)
    except:
        positions_list=[]
    return positions_list

df['Positions_spike_mutations']=df['spike_mutations'].apply(lambda x: determine_positions(x))
print(df)

def nt_change_positions(nt_changes):

    nt_changes_list=nt_changes.rsplit(',')
    #print(nt_changes_list)
    positions_list=[]
    for x in nt_changes_list:
        pos=int(x[1:-1])
        positions_list.append(pos)

    return positions_list

df['Positions_all_nt_changes']=df['substitutions'].dropna().apply(lambda x: nt_change_positions(x))

def count_RBD(positions_list):
    RBD_count = 0
    try:
        for x in positions_list:
            if 328 <= x <= 533:
                RBD_count+=1
    except:
        RBD_count=0
    return RBD_count

def count_cleavage(positions_list):
    cleavage_count = 0
    try:
        for x in positions_list:
            if 680 <= x <= 744:
                cleavage_count+=1
            elif 782 <= x <= 814:
                cleavage_count+=1
    except:
        cleavage_count=0

    return cleavage_count

def count_spike_nt_changes(positions_list):
    spike_nt_changes_count = 0
    try:
        for x in positions_list:
            if 21563 <= x <= 25384:
                spike_nt_changes_count+=1
    except:
        spike_nt_changes_count=0

    return spike_nt_changes_count

df['Num_RBD_mutations']=df['Positions_spike_mutations'].apply(lambda x: count_RBD(x))

df['Num_protease_cleavage_mutations']=df['Positions_spike_mutations'].apply(lambda x: count_cleavage(x))

df['Num_spike_nt_changes']=df['Positions_all_nt_changes'].apply(lambda x: count_spike_nt_changes(x))
print(df)

df.to_excel('501Y.V2_n344_spike_mutations_quantification.xlsx')

spike_variant_list=df['spike_mutations'].dropna().tolist()
print(spike_variant_list)
flat_list = [item for sublist in spike_variant_list for item in sublist]

mutation_df=pd.DataFrame(flat_list)
mutation_df.columns=['mutation']
mutation_df=mutation_df.replace({'':np.nan})
mutation_df=mutation_df.dropna()
#print(mutation_df)
#mutation_df.to_csv('test_mutation_list.csv', index=False, header=True)
print(mutation_df['mutation'].unique().tolist())

mutation_df['location']=mutation_df['mutation'].apply(lambda x: int(x[3:-1]))
print(mutation_df)

mutation_df.to_csv('501Y.V2_n344_spike_mutations_table.csv', index=False, header=True)
