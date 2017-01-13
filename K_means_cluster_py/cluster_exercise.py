import pandas as pd
from pandas import DataFrame
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import pylab as p
import matplotlib.cm as cm
import numpy as np
import math
import matplotlib.pyplot as plt

# read file
tetra_df = pd.read_csv('Tetra.lrn',skiprows=[0,1,2],delimiter='\t',index_col=None, usecols=[0,1,2,3])

## normalize the columns using a simple function
def normCols( df_in , a , b):
    "This takes a dataframe and normalizes the columns"
    output_df = df_in.copy()
    for colNum in range(a,b):
        max1 = max(tetra_df.ix[:, colNum])
        min1 = min(tetra_df.ix[:, colNum])
        diff1 = max1-min1
        output_df.ix[:, colNum] = output_df.ix[:, colNum] - min1
        output_df.ix[:, colNum] = output_df.ix[:, colNum] / diff1
    return output_df

# call the function to get normalized data
norm_df = normCols(tetra_df,1,4)

# I usually see results of 3-14 iterations!
## k means function: dataframe 'df' and number of clusters 'k'
def k_means(df, k):
    "Runs basic k-means algorithm"
    # create column in the dataset for cluster number
    newCol = pd.DataFrame(0, index=range(400), columns=['kNum'])
    new_df = pd.concat([df, newCol], axis=1)

    # randomize initial values
    prev_mu = np.zeros((k,3))
    curr_mu = np.random.random((k, 3))

    ## check for rare condition that empty initial cluster exist
    for numRow in range(0, new_df.shape[0]):
        curr_value = new_df.ix[numRow, [1, 2, 3]].as_matrix()
        dist_comp = np.zeros(k)
        for kcount in range(0, k):
            dist_comp[kcount] = np.linalg.norm(curr_mu[kcount] - curr_value)

        new_df.ix[numRow, 4] = np.where(dist_comp == dist_comp.min())[0][0] + 1

    clusterCheck = new_df.ix[:, 4]
    # while one is empty, re-randomize cluster centroid
    while (clusterCheck[clusterCheck.isin([1])].empty or clusterCheck[clusterCheck.isin([2])].empty or clusterCheck[clusterCheck.isin([3])].empty or clusterCheck[clusterCheck.isin([4])].empty):
        curr_mu = np.random.random((k, 3))

        for numRow in range(0, new_df.shape[0]):
            curr_value = new_df.ix[numRow, [1, 2, 3]].as_matrix()
            dist_comp = np.zeros(k)
            for kcount in range(0, k):
                dist_comp[kcount] = np.linalg.norm(curr_mu[kcount] - curr_value)

            new_df.ix[numRow, 4] = np.where(dist_comp == dist_comp.min())[0][0] + 1


    # keep track of number of iterations
    countTracker = 0
    ## main iteration process
    while (not np.array_equal(prev_mu,curr_mu)):
        countTracker += 1
        for numRow in range(0,new_df.shape[0]):
            curr_value = new_df.ix[numRow,[1,2,3]].as_matrix()
            dist_comp = np.zeros(k)
            for kcount in range(0,k):
                dist_comp[kcount] = np.linalg.norm(curr_mu[kcount] - curr_value)

            new_df.ix[numRow, 4] = np.where(dist_comp == dist_comp.min())[0][0] + 1

        # update point values
        prev_mu = curr_mu.copy()
        for kcount in range(0,k):
            temp_df = new_df.loc[new_df['kNum'] == (kcount+1)]

            curr_mu[kcount, 0] = np.mean(temp_df.ix[:, 1])
            curr_mu[kcount, 1] = np.mean(temp_df.ix[:, 2])
            curr_mu[kcount, 2] = np.mean(temp_df.ix[:, 3])

    print("***{}{}{}***".format("Completed with ", countTracker, " number of iterations."))
    return [curr_mu, new_df]


[centroid_mat, final_df] = k_means(norm_df,4)

print(centroid_mat)

## Plot the clusters
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

colormap = plt.cm.gist_ncar #nipy_spectral, Set1,Paired
colorst = [colormap(i) for i in np.linspace(0, 0.9,len(ax.collections))]
for t,j1 in enumerate(ax.collections):
    j1.set_color(colorst[t])

ax.scatter(final_df.ix[:,1], final_df.ix[:,2], final_df.ix[:,3], c=final_df.ix[:,4], marker='o')

ax.set_xlim3d(0, 1)
ax.set_ylim3d(0, 1)
ax.set_zlim3d(0, 1)

ax.set_xlabel('X-axis')
ax.set_ylabel('Y-axis')
ax.set_zlabel('Z-axis')

plt.show()
