import pandas as pd
import numpy as np

mutation_list=[]
with open('501_sequences_n341_nucleotide_mutations.txt') as f:

    for line in f:
        #print(line)
        mutation=line.rstrip().split(',')
        mutation_list.append(mutation)
#print(mutation_list)

flat_list = [item for sublist in mutation_list for item in sublist]
#print(flat_list)

def remove_values_from_list(the_list, val):
   return [value for value in the_list if value != val]



mutation_df=pd.DataFrame(flat_list)
mutation_df.columns=['mutation']
mutation_df=mutation_df.replace({'':np.nan})
mutation_df=mutation_df.dropna()
#print(mutation_df)
#mutation_df.to_csv('test_mutation_list.csv', index=False, header=True)
print(mutation_df['mutation'].unique().tolist())

mutation_df['location']=mutation_df['mutation'].apply(lambda x: int(x[1:-1]))
#print(mutation_df)

mutation_df.to_csv('501_sequences_n341_nucleotide__mutations_table.csv', index=False, header=True)
