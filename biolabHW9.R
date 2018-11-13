#1

library(microbenchmark)

odd_count = function(x) 
{
  odd_num = 0
  for(i in 1:length(x)) 
  {
    if(x[i] %% 2 == 1) 
      odd_num = odd_num + 1
  }
  return(odd_num)
}

odd_count_vec = function(x) 
{
  
  return(sum(x %% 2 == 1))
}

microbenchmark(odd_count_vec, odd_count, times = 1000L)

#2

sort_vect = function(x, ascending = TRUE) 
{
  if(length(x) < 2)
    return(x)
  for(last in length(x):2) 
  {
    for(first in 1:(last - 1)) 
    {
      if((x[first] > x[first + 1]) == ascending) 
      {
        temp = x[first]
        x[first] = x[first + 1]
        x[first + 1] = temp
      }
    }
  }
  return(x)
}

sort_vect(c(3, 1, 2), ascending = TRUE)
sort_vect(c(3, 1, 2), ascending = FALSE)


#3

N = 1000000 # I have also tried N = 1000 and 10000.

#dynamically allocated memory
data_series = 0 
system.time({for (i in 2:N) { 
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) 
  } 
})

#preallocated memory
data_series1 = vector("numeric", N)
system.time({
  for (i in 2:N) {
    data_series1[i] = data_series1[i-1] + sample(c(-1, 1), 1)
  }
})

# It appears that when N = 1000, there is no obvious difference. When N = 10000, the second one used 0.01 seconds less than the first one (0.05 and 0.06).
# When N = 1000000, the second one runs obviously faster than the first one (4.49 and 5.07).