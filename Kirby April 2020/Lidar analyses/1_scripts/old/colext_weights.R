# ----------------------------------------------------------------------------
# Create the spatial weights matrix for the spatial bird community model
# sensu Mattsson et al. (2012)
# ----------------------------------------------------------------------------


library(rgdal)
library(spdep)
library(reshape2)

rm(list=ls(all=TRUE))

# Set the working directory to the library containing the data and shapefile
#workdir<-""
#setwd(workdir)
#Error in setwd(workdir) : cannot change working directory  ******************************************************

# Bring detection data and format it for detection/site
# --------------------------------------------------------------------
y.data<-read.csv("recast.BBWA.csv", header=T)
head(y.data)

R<-nrow(y.data) 			# The number of sites
J<-4				# The number of replicate surveys
K<-(ncol(y.data)-1)/4				# The number of years  


  
# Format the detection data into a site x survey x year array
y.year.data<-array(NA, dim=c(R, J, K))
for(r in 1:R){
  for(j in 1:J){
    x=2+j-1
    for(k in 1:K){
	y.year.data[r,j,k]<-y.data[r,x]
	x=x+J
	}
    }
  }
y.year.data[,,3]

# Create the site x year detection matrix
site_det<-matrix(NA, R, K)
for(r in 1:R){
  for(k in 1:K){
    site_det[r,k]<-min(1, max(y.year.data[r,,k], na.rm=TRUE))
    }
  }
head(site_det)
write.csv(site_det, file="site_det.csv")

# Bring in the plot location data and create the neighborhood matrices
# --------------------------------------------------------------------
points<-readOGR(dsn="C:/Users/Lionel/Documents/MITACS ACCELERATE FELLOWSHIP/NAFE paper/June 2017", layer="NAFEmsompointcounts")
#Error in readOGR(dsn = workdir, layer = "NAFEmsompointcounts") : *************************************************
#  empty name


class(points)
head(points)

coords<-coordinates(points)
plot(points)

# 600-m neighborhood: Identify neighbors by Euclidean distance
nb_600<-dnearneigh(points, d1=0, d2=600, row.names=points$SS)

# Create the 600-meter neighborhood matrix
nb_600_wb<-nb2mat(nb_600, style="B", zero.policy=TRUE)
row.names(nb_600_wb)
write.csv(nb_600_wb, file="nb_600_wb.csv")  #data check


# Creating the spatial weights matrix - a site-by-year matrix where each
# cell contains the average number of sites neighboring site r where the
# species was detected in year k. 
# --------------------------------------------------------------------------

# Create the 600-meter neighborhood weights matrix
nsites<-dim(nb_600_wb)[1]

num_neigh<-rep(NA, nsites)
for(i in 1:nsites){
  num_neigh[i]<-sum(nb_600_wb[i,])
  }
length(which(num_neigh==0))

sum_neighbors<-sum(num_neigh)

site_year_weights_600<-matrix(0, R, K)
for(k in 1:K){
  for(r in 1:R){
    if(num_neigh[r]==0){
	site_year_weights_600[r,k]<-0
    }else{
	d<-0
	N<-num_neigh[r]
	for(j in 1:R){
	  if(nb_600_wb[r,j]==0){
	    temp<-0
	  }else{
	    temp<-site_det[j,k]
	    }
	  d<-d+temp
	  }
	site_year_weights_600[r,k] <- d/N
	}
  }
}

site_year_weights_600

write.csv(site_year_weights_600, "BBWA_weights_600.csv")

BBWA_weights_600<-read.csv("BBWA_weights_600.csv", header=T)
str(BBWA_weights_600)
#need to change to matrix, transpose matrix, then stack the transposed columns?
BBWA_weights_600.m<-as.matrix(BBWA_weights_600[,2:20])
BBWA_weights_600.t<-t(BBWA_weights_600.m)
BBWA_weights_600.tdf<-as.data.frame(BBWA_weights_600.t)
BBWA_weights_600.stacked<-stack(BBWA_weights_600.tdf)
str(BBWA_weights_600.stacked)
BBWA_weights_600.stacked<-BBWA_weights_600.stacked 
write.csv(BBWA_weights_600.stacked, "BBWA_weights_600.stacked.csv")


# 400-m neighborhood: Identify neighbors by Euclidean distance
nb_400<-dnearneigh(points, d1=0, d2=400, row.names=points$SS)

# Create the 400-meter neighborhood matrix
nb_400_wb<-nb2mat(nb_400, style="B", zero.policy=TRUE)
row.names(nb_400_wb)

# Creating the spatial weights matrix - a site-by-year matrix where each
# cell contains the average number of sites neighboring site r where the
# species was detected in year k. 
# --------------------------------------------------------------------------

# Create the 400-meter neighborhood weights matrix
nsites<-dim(nb_400_wb)[1]

num_neigh<-rep(NA, nsites)
for(i in 1:nsites){
  num_neigh[i]<-sum(nb_400_wb[i,])
}
length(which(num_neigh==0))

sum_neighbors<-sum(num_neigh)

site_year_weights_400<-matrix(0, R, K)
for(k in 1:K){
  for(r in 1:R){
    if(num_neigh[r]==0){
      site_year_weights_400[r,k]<-0
    }else{
      d<-0
      N<-num_neigh[r]
      for(j in 1:R){
        if(nb_400_wb[r,j]==0){
          temp<-0
        }else{
          temp<-site_det[j,k]
        }
        d<-d+temp
      }
      site_year_weights_400[r,k] <- d/N
    }
  }
}

site_year_weights_400

write.csv(site_year_weights_400, "BBWA_weights_400.csv")

BBWA_weights_400<-read.csv("BBWA_weights_400.csv", header=T)
str(BBWA_weights_400)
#need to change to matrix, transpose matrix, then stack the transposed columns?
BBWA_weights_400.m<-as.matrix(BBWA_weights_400[,2:20])
BBWA_weights_400.t<-t(BBWA_weights_400.m)
BBWA_weights_400.tdf<-as.data.frame(BBWA_weights_400.t)
BBWA_weights_400.stacked<-stack(BBWA_weights_400.tdf)
str(BBWA_weights_400.stacked)
BBWA_weights_400.stacked<-BBWA_weights_400.stacked 
write.csv(BBWA_weights_400.stacked, "BBWA_weights_400.stacked.csv")



# 200-m neighborhood: Identify neighbors by Euclidean distance
nb_200<-dnearneigh(points, d1=0, d2=250, row.names=points$SS)#Note that d2>200 to make sure adjacent point counts are counted

# Create the 200-meter neighborhood matrix
nb_200_wb<-nb2mat(nb_200, style="B", zero.policy=TRUE)
row.names(nb_200_wb)

# Creating the spatial weights matrix - a site-by-year matrix where each
# cell contains the average number of sites neighboring site r where the
# species was detected in year k. 
# --------------------------------------------------------------------------

# Create the 200-meter neighborhood weights matrix
nsites<-dim(nb_200_wb)[1]

num_neigh<-rep(NA, nsites)
for(i in 1:nsites){
  num_neigh[i]<-sum(nb_200_wb[i,])
}
length(which(num_neigh==0))

sum_neighbors<-sum(num_neigh)

site_year_weights_200<-matrix(0, R, K)
for(k in 1:K){
  for(r in 1:R){
    if(num_neigh[r]==0){
      site_year_weights_200[r,k]<-0
    }else{
      d<-0
      N<-num_neigh[r]
      for(j in 1:R){
        if(nb_200_wb[r,j]==0){
          temp<-0
        }else{
          temp<-site_det[j,k]
        }
        d<-d+temp
      }
      site_year_weights_200[r,k] <- d/N
    }
  }
}

site_year_weights_200

write.csv(site_year_weights_200, "BBWA_weights_200.csv")

BBWA_weights_200<-read.csv("BBWA_weights_200.csv", header=T)
str(BBWA_weights_200)
#need to change to matrix, transpose matrix, then stack the transposed columns?
BBWA_weights_200.m<-as.matrix(BBWA_weights_200[,2:20])
BBWA_weights_200.t<-t(BBWA_weights_200.m)
BBWA_weights_200.tdf<-as.data.frame(BBWA_weights_200.t)
BBWA_weights_200.stacked<-stack(BBWA_weights_200.tdf)
str(BBWA_weights_200.stacked)
BBWA_weights_200.stacked<-BBWA_weights_200.stacked 
write.csv(BBWA_weights_200.stacked, "BBWA_weights_200.stacked.csv")
