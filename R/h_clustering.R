# Distance function to compute a full Euclidean distance matrix for
# the input matrix x to measure the dissimilarities. The output[i,j] gives 
# the Euclidean distance between rows x[i,] and x[j,].
distance = function(x)
{
  x = as.matrix(x)
  u = apply(x*x,1,sum) %*% matrix(1.0,1,nrow(x))
  sqrt(abs(u + t(u) - 2 * x %*% t(x)))
}

# The function returns a vector giving the permutation of the original
# observations suitable 
# for plotting, in the sense that a cluster plot using this ordering and matrix
# will not have crossings of the branches.
hclust_order = function(mat)
{
  N = nrow(mat) + 1
  hclust_order = rep(0,N)
  hclust_order[1] = mat[N-1,1]
  hclust_order[2] = mat[N-1,2]
  loc = 2
  for(i in seq(N-2,1))
  {
    for(j in seq(1,loc))
    {
      if(hclust_order[j] == i)
      {
        hclust_order[j] = mat[i,1]
        if(j==loc)
        {
          loc = loc + 1
          hclust_order[loc] = mat[i,2]
        } else
        {
          loc = loc + 1
          for(k in seq(loc, j+2)) hclust_order[k] = hclust_order[k-1]
          hclust_order[j+1] = mat[i,2]
        }
      }
    }
  }
  -hclust_order
}

#hierarchical clustering implementation.
# be used like hclust(d, method).
hierclust = function(d, method=c("single","complete","average","median"))
{
  if(!is.matrix(d)) d = as.matrix(d)
  # defining clustering functions
  method_cluster = switch(match.arg(method),
                     single   = min,
                     complete = max,
                     average  = mean,
                     median  = median)
  Nrows = nrow(d)
  diag(d)=Inf
  n = -(1:Nrows)                       # for group membership
  m = matrix(0,nrow=Nrows-1, ncol=2)   # merge output of hclust function
  # an n−1 by 2 matrix. Row i of merge describes the merging of clusters at step 
  # i of the clustering.
  # If an element j in the row is negative, then observation -j was merged
  # at this stage. If j is positive then the merge was with the cluster 
  # formed at the (earlier) stage j of the algorithm.
  # Thus negative entries in merge indicate agglomerations of singletons,
  # and positive entries indicate agglomerations of non-singletons.
  h = rep(0,Nrows-1)                   # hclust's height output
  for(j in seq(1,Nrows-1))
  {
    # Calculates the smallest distance
    # and its corresponding indices
    h[j] = min(d)
    i = which(d - h[j] == 0, arr.ind=TRUE)
    # merging the first pair.
    i = i[1,,drop=FALSE]
    p = n[i]
    # order each m[j,] pair
    p = p[order(p)]
    m[j,] = p
    # Agglomerate the previous groups and the current pair 
    # into the current jth group
    # (Agglomerative clustering)
    grp = c(i, which(n %in% n[i[1,n[i]>0]]))
    n[grp] = j
    # Applying chosen method distance to replacement distances
    r = apply(d[i,],2,method_cluster)
    # Set the pointer to the next minimum distance, 
    # excluding current one by modifying
    # the distance matrix
    d[min(i),] = d[,min(i)] = r
    d[min(i),min(i)]        = Inf
    d[max(i),] = d[,max(i)] = Inf
  }
  # Return the object as similar to the output from hclust.
  
  # labels for each of the objects being clustered
  # call which produced the result
  # method is the cluster method used
  # order is a vector giving the permutation of the original observations
  # suitable for plotting, in the sense that a cluster plot using
  # this ordering and matrix merge will not have crossings of the branches
  # height is a set of n−1 real values (non-decreasing for ultrametric trees).
  # The clustering height is the value of the criterion associated 
  # with the clustering method for the particular agglomeration.
  # merge is the n-1 by 2 matrix
  structure(list(merge = m, height = h, order = hclust_order(m),
                 labels = rownames(d), method = method, 
                 call = match.call(), dist.method = "euclidean"), 
            class = "hclust")
}

# Plot implemented hierclust() and defined R function hclust()
# R defined hclust() function for hierarchical clustering
h = hclust(dist(USArrests),method="single")
# Our implemented agglomerative hierarchical clustering method
h_clust = hierclust(distance(USArrests), method="single")
plot(h)
plot(h_clust)