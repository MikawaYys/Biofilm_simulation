
simulationtimes=2

for(batch in c(2))
{
  dir='/Users/yeyusong/Desktop/德国工作 project/Cell-matrix/results/movies_of_biofilm'
  setwd(dir) 
  
  dir.create(paste0('S=',batch))

  dirsave=paste0('/Users/yeyusong/Desktop/德国工作 project/Cell-matrix/results/movies_of_biofilm/',paste0('S=',batch))
  setwd(dirsave) 

#set direction 



#define parametersets
size_matrix=100
lattice_length=15
kbt=1


realtime=0


#calculating rates function

#cells
doubletime=2
doubletimesec=doubletime*3600
rateofdiv=1/(1*doubletimesec)*1
#matrix
rateofsecret=1/(1*doubletimesec)*1

#agar
rateofagardrift=3*10^(-5)*4*1/((lattice_length/size_matrix)^2)
#cell
rateofcelldrift=5.625*10^(-7)*4*1/((lattice_length/size_matrix)^2)*0.4
#matrix
rateofmatrixdrift=5.625*10^(-7)*4*1/((lattice_length/size_matrix)^2)*2.5


#fraction of rates
division_rate=rateofdiv/(rateofdiv+rateofsecret+rateofcelldrift+rateofmatrixdrift+rateofagardrift)
secrete_rate=rateofsecret/(rateofdiv+rateofsecret+rateofcelldrift+rateofmatrixdrift+rateofagardrift)
drift_rate=(rateofcelldrift+rateofmatrixdrift+rateofagardrift)/(rateofdiv+rateofsecret+rateofcelldrift+rateofmatrixdrift+rateofagardrift)
drift_cell_rate=rateofcelldrift/(rateofcelldrift+rateofmatrixdrift+rateofagardrift+rateofdiv+rateofsecret)
drift_matrix_rate=rateofmatrixdrift/(rateofcelldrift+rateofmatrixdrift+rateofagardrift+rateofdiv+rateofsecret)
drift_agar_rate=rateofagardrift/(rateofcelldrift+rateofmatrixdrift+rateofagardrift+rateofdiv+rateofsecret)


freeze_agar=1

#basice physical steps
basictimestep=1/(rateofdiv+rateofsecret+rateofcelldrift+rateofmatrixdrift+rateofagardrift)/3600

#cut the biofilm (hour)
cutting_time=24

ifcut=10000

#probability of leaving cells
ifleftcells=0.0


#cutting a quorter
ifsqr=0
coorx1=0
coorx2=size_matrix/2
coory1=size_matrix/2+1
coory2=size_matrix

#cutting a round
ifround=0
round_r=13
roundcenter_x=size_matrix/2
roundcenter_y=size_matrix/2

#cell plant
ifputnewcell=0
coorx1_newcell=50
coorx2_newcell=51
coory1_newcell=75
coory2_newcell=76


#if refrashagar

refrashagar=0

#statistical region
cutline_cell=matrix(0,nrow=size_matrix,ncol=size_matrix)
cutline_matrix=matrix(0,nrow=size_matrix,ncol=size_matrix)
cutrange=3
cutline1y=25
cutline2y=45
cutline3y=65

#biomasshisto
edge=65
biohigh=10
drawingwidthL=5
drawingwidthR=size_matrix-5
interx=30
intery=2

#set color
IndianRed=rgb(255/255,106/255,106/255)
RoyalBlue=rgb(65/255,105/255,225/255)

#set death rate (fixed?)
base_deathrate=1.1*5.5*0.000001*basictimestep*3600*1
base_deathrate_2=5.5/2*0.000001*basictimestep*3600*1*2.1

limit_of_cell_layer=100
limit_of_matrix_layer=100

#max layers
max_agar=3

#steps of recording
moviestep_biofilm=10000
totalsteps_biofilm=25000000


total_physical_time=seq(1,totalsteps_biofilm/moviestep_biofilm,1)


#show the cut
#window_of_average=2


#three weight parameters
c_weight=0.1
#c-c
c1_weight=1
#c-m
c2_weight=1
#m-m
c3_weight=1

j_weight=1

n1_weight=1.5
n2_weight=1



NAME='000000.pdf'


total_cell=0
total_matrix=0

index_cell_i=c()
index_cell_j=c()
indexcell=0
age_cell=c()
live_or_death=c()


index_matrix_i=c()
index_matrix_j=c()
indexmatrix=0


lattice_cell=matrix(0,nrow=size_matrix, ncol=size_matrix)
lattice_cell_new=matrix(0,nrow=size_matrix, ncol=size_matrix)

lattice_deathcell=matrix(0,nrow=size_matrix, ncol=size_matrix)

lattice_matrix=matrix(0,nrow=size_matrix, ncol=size_matrix)
lattice_matrix_new=matrix(0,nrow=size_matrix, ncol=size_matrix)

lattice_agar=matrix(0,nrow=size_matrix, ncol=size_matrix)

round_agar=size_matrix

coorxvec=c()
cooryvec=c()
for(coorx_i in 1:size_matrix)
{
  for(coory_j in 1:size_matrix)
  {
    if((abs(coorx_i-roundcenter_x)^2+abs(coory_j-roundcenter_y)^2)^0.5<round_agar)
    {
      coorxvec=c(coorxvec,coorx_i)
      cooryvec=c(cooryvec,coory_j)
    }
  }
}

for(cutsite in 1:length(coorxvec))
{
  coorx=coorxvec[cutsite]
  coory=cooryvec[cutsite]
  
  lattice_agar[coorx,coory]=max_agar
  
}



#1 cell 2 matrix 3 deathcell
coefficientmatrix_biofilm=matrix(0,nrow=3,ncol=3)
coefficientmatrix_biofilm[1,1]=0
coefficientmatrix_biofilm[1,2]=0.03

coefficientmatrix_biofilm[2,1]=0.03
coefficientmatrix_biofilm[2,2]=0

coefficientmatrix_biofilm[1,3]=0
coefficientmatrix_biofilm[3,1]=0


interaction_cell_death=0


#initial biomass configuration

drop_dense=1

R_of_initial=2.5


coorxvec=c()
cooryvec=c()
for(coorx_i in 1:size_matrix)
{
  for(coory_j in 1:size_matrix)
  {
    if((abs((coorx_i-roundcenter_x)*1)^2+abs((coory_j-roundcenter_y)*1)^2)^0.5<R_of_initial & runif(1,0,1)<drop_dense)
    {
      coorxvec=c(coorxvec,(coorx_i-roundcenter_x)*4+roundcenter_x)
      cooryvec=c(cooryvec,(coory_j-roundcenter_y)*4+roundcenter_y)
    }
  }
}

for(cutsite in 1:length(coorxvec))
{
  coorx=coorxvec[cutsite]
  coory=cooryvec[cutsite]
  
  lattice_cell[coorx,coory]=1
  indexcell=indexcell+1
  index_cell_i=c(index_cell_i,coorx)
  index_cell_j=c(index_cell_j,coory)
  age_cell=c(age_cell,0)
  live_or_death=c(live_or_death,0)
  total_cell=total_cell+1
  
}
lattice_cell_new=lattice_cell


# L_of_initial=10
# 
# 
# for(cutsite in 1:length(coorxvec))
# {
#   coorx=coorxvec[cutsite]
#   coory=cooryvec[cutsite]
#   
#   lattice_matrix[coorx,coory]=1
#   indexmatrix=indexmatrix+1
#   index_matrix_i=c(index_matrix_i,coorx)
#   index_matrix_j=c(index_matrix_j,coory)
#   
#   total_matrix=total_matrix+1
# }


#for(i in (size_matrix/2-L_of_initial+1):(size_matrix/2+L_of_initial+1))
#{
# for(j in (size_matrix/2-L_of_initial+1):(size_matrix/2+L_of_initial+1))
#{
# {
#   lattice_matrix[i,j]=1
#   indexmatrix=indexmatrix+1
#   index_matrix_i=c(index_matrix_i,i)
#   index_matrix_j=c(index_matrix_j,j)
#  age_cell=c(age_cell,0)
#  live_or_death=c(live_or_death,0)
#  total_matrix=total_matrix+1
# }
#}
#}
lattice_matrix_new=lattice_matrix

pdf(file='drop.pdf',width=9,height=9)

par(mar=c(1,1,1,1))
max_celllayer=max(lattice_cell)
max_matrixlayer=max(lattice_matrix)
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="n")

for(i in 1:size_matrix)
{
  for(j in 1:size_matrix)
  {
    par(new=TRUE)
    if(lattice_matrix[i,j]!=0)
      polygon(c(j-1,j,j,j-1),c(size_matrix-i,size_matrix-i,size_matrix-i+1,size_matrix-i+1), density = NULL, border = F, col = rgb(1-lattice_matrix[i,j]/max_matrixlayer*190/255,1-lattice_matrix[i,j]/max_matrixlayer*150/255,1-lattice_matrix[i,j]/max_matrixlayer*30/255))
  }
}

for(i in 1:size_matrix)
{
  for(j in 1:size_matrix)
  {
    par(new=TRUE)
    if(lattice_cell[i,j]!=0)
      polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size_matrix-i+0.2,size_matrix-i+0.2,size_matrix-i+1-0.2,size_matrix-i+1-0.2), density = NULL, border = F, col = rgb(1,1-lattice_cell[i,j]/max_celllayer,1-lattice_cell[i,j]/max_celllayer))
  }
}

par(new=T)
f=seq(0,2*pi,0.001)
x_cir=sin(f)*0.3/1.5*50+49.5
y_cir=cos(f)*0.3/1.5*50+50.5
plot(x_cir,y_cir,type="l",tck=0.03,cex=0.5,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
while (!is.null(dev.list()))  dev.off()


#wound radius new cells
if_round_stac=1
points_of_radius=10
wound_radius=matrix(rep(0,points_of_radius))
wound_radius_cell=matrix(rep(0,points_of_radius))
wound_radius_matrix=matrix(rep(0,points_of_radius))
wound_radius_death=matrix(rep(0,points_of_radius))
interval_of_radius=seq(0,size_matrix/2,size_matrix/2/points_of_radius)



#simulation

#simulation and create movie
Sys.time()
successunmber_cell=sum(lattice_cell)
successunmber_matrix=sum(lattice_matrix)
successunmber_cellmove=0
successunmber_matrixmove=0


countsteps_biofilm=totalsteps_biofilm/moviestep_biofilm

TL_successunmber_cell=c()
TL_successunmber_matrix=c()
TL_successunmber_cellmove=c()
TL_successunmber_matrixmove=c()

Total_death_cell=c()


#record_cell_matrix

record_cell=matrix(0,1,size_matrix)
record_matrix=matrix(0,1,size_matrix)

record_death=matrix(0,1,size_matrix)
record_agar=matrix(0,1,size_matrix)


record_index=1
record_time=1

#freq of production
freq_record_time=48
recordon=0

freq_of_prod=matrix(0,size_matrix,size_matrix)
record_freq_of_prod=matrix(0,1,size_matrix)
freq_of_diff=matrix(0,size_matrix,size_matrix)
record_freq_of_diff=matrix(0,1,size_matrix)

#define function
#old new site
choose_newsite_drift <- function(old_i,old_j)
{
  neibor_j=c(old_j-1,old_j-1,old_j-1,old_j,old_j+1,old_j+1,old_j+1,old_j)
  neibor_i=c(old_i-1,old_i,old_i+1,old_i+1,old_i+1,old_i,old_i-1,old_i-1)
  new_site=sample(1:8,size=1)
  new_j=neibor_j[new_site]
  new_i=neibor_i[new_site]
  return(c(new_i,new_j))
}

choose_newsite_agardrift <- function(old_i,old_j)
{
  
  new_j=sample((old_j-1):(old_j+1),size=1)
  new_i=sample((old_i-1):(old_i+1),size=1)
  return(c(new_i,new_j))
}

choose_newsite_create <- function(old_i,old_j)
{
  neibor_j=c(old_j-1,old_j-1,old_j-1,old_j,old_j+1,old_j+1,old_j+1,old_j,old_j)
  neibor_i=c(old_i-1,old_i,old_i+1,old_i+1,old_i+1,old_i,old_i-1,old_i-1,old_i)
  new_site=sample(1:9,size=1)
  new_j=neibor_j[new_site]
  new_i=neibor_i[new_site]
  return(c(new_i,new_j))
}

#calculation energy
calculation_energy <- function(celltype,site_i,site_j,Matrix_of_cell,Matrix_of_matrix,size_matrix)
{
  neibor_j=c(site_j-1,site_j-1,site_j-1,site_j,site_j+1,site_j+1,site_j+1,site_j,site_j)
  neibor_i=c(site_i-1,site_i,site_i+1,site_i+1,site_i+1,site_i,site_i-1,site_i-1,site_i)
  countofcell=0
  countofmatrix=0
  neighber_of_deathcell=0
  for(countsite in 1:9)
  {
    if(neibor_i[countsite]>0 & neibor_i[countsite]<size_matrix+1 & neibor_j[countsite]>0 & neibor_j[countsite]<size_matrix+1)
    {
      countofcell=countofcell+Matrix_of_cell[neibor_i[countsite],neibor_j[countsite]]
      neighber_of_deathcell=neighber_of_deathcell+lattice_deathcell[neibor_i[countsite],neibor_j[countsite]]
    }
  }
  for(countsite in 1:9)
  {
    if(neibor_i[countsite]>0 & neibor_i[countsite]<size_matrix+1 & neibor_j[countsite]>0 & neibor_j[countsite]<size_matrix+1)
      countofmatrix=countofmatrix+Matrix_of_matrix[neibor_i[countsite],neibor_j[countsite]]
  }
  
  energy=(countofcell-neighber_of_deathcell)*coefficientmatrix_biofilm[celltype,1]+neighber_of_deathcell*interaction_cell_death+countofmatrix*coefficientmatrix_biofilm[celltype,2]
  
  return(energy)
}

#calculation capacity
calculation_capacity <- function(celltype,site_i,site_j,Matrix_of_cell,Matrix_of_matrix,size_matrix)
{
  #neibor_j=c(site_j-1,site_j-1,site_j-1,site_j,site_j+1,site_j+1,site_j+1,site_j,site_j)
  #neibor_i=c(site_i-1,site_i,site_i+1,site_i+1,site_i+1,site_i,site_i-1,site_i-1,site_i)
  countofcell=0
  countofmatrix=0
  #for(countsite in 1:9)
  #{
  #  if(neibor_i[countsite]>0 & neibor_i[countsite]<sizematrix+1 & neibor_j[countsite]>0 & neibor_j[countsite]<sizematrix+1)
  #    countofcell=countofcell+Matrix_of_cell[neibor_i[countsite],neibor_j[countsite]]
  #}
  #for(countsite in 1:9)
  #{
  #  if(neibor_i[countsite]>0 & neibor_i[countsite]<sizematrix+1 & neibor_j[countsite]>0 & neibor_j[countsite]<sizematrix+1)
  #    countofmatrix=countofmatrix+Matrix_of_matrix[neibor_i[countsite],neibor_j[countsite]]
  #}
  countofcell=Matrix_of_cell[site_i,site_j]
  countofmatrix=Matrix_of_matrix[site_i,site_j]
  if(celltype==1)
  {
    probability_of_capacity=(c1_weight*countofcell+c2_weight*countofmatrix)
  }
  if(celltype==2)
  {
    probability_of_capacity=(c2_weight*countofcell+c3_weight*countofmatrix)
  }
  return(probability_of_capacity)
}


#calculation deathrate
calculation_deathrate <- function(age_1,base_deathrate_1,base_death_rate_2,site_i,site_j)
{
  neibor_j=c(site_j-1,site_j-1,site_j-1,site_j,site_j+1,site_j+1,site_j+1,site_j,site_j)
  neibor_i=c(site_i-1,site_i,site_i+1,site_i+1,site_i+1,site_i,site_i-1,site_i-1,site_i)
  countofdeathcell=0
  for(countsite in 1:9)
  {
    if(neibor_i[countsite]>0 & neibor_i[countsite]<size_matrix+1 & neibor_j[countsite]>0 & neibor_j[countsite]<size_matrix+1)
    {
      countofdeathcell=countofdeathcell+lattice_deathcell[neibor_i[countsite],neibor_j[countsite]]
    }
  }
  
  
  death_rate_1=base_deathrate_1+countofdeathcell*base_death_rate_2
  return(death_rate_1)
}

#calculation Nutrient
calculation_agar <- function(lattice_agar1,site_i,site_j)
{
  return((max_agar-lattice_agar1[site_i,site_j]))
}

#start simulation

for(steps in 1:totalsteps_biofilm)
{
  
  total_agar=sum(lattice_agar)
  twolayer=runif(1,0,1)

  balancing_quantity_matrix=successunmber_matrix/successunmber_cell
  balancing_quantity_agar=total_agar/successunmber_cell
  totalrate=(division_rate+secrete_rate+drift_cell_rate+drift_matrix_rate*balancing_quantity_matrix+drift_agar_rate)
  
  if(twolayer<(division_rate)/totalrate) 
    # Division
  {
    indexcell=sample(1:total_cell,size=1)
    
    
    
    old_i=index_cell_i[indexcell]
    old_j=index_cell_j[indexcell]
    age=age_cell[indexcell]
    death_rate=calculation_deathrate(age,base_deathrate,base_deathrate_2,old_i,old_j)
    
    
    if(runif(1,0,1)<death_rate & live_or_death[indexcell]==0)
    {
      live_or_death[indexcell]=1
      lattice_deathcell[old_i,old_j]=lattice_deathcell[old_i,old_j]+1
    }
    
    if(live_or_death[indexcell]==0)
    {
      
      
      new_ij=choose_newsite_create(old_i,old_j)
      new_i=new_ij[1]
      new_j=new_ij[2]
      
      if(new_j>0 & new_j<(size_matrix+1) & new_i>0 & new_i<(size_matrix+1))
      {
        if(lattice_cell[new_i,new_j]<(limit_of_cell_layer) & lattice_agar[old_i,old_j]>1)
        {
          #energy
          energy_capacity=calculation_capacity(1,new_i,new_j,lattice_cell,lattice_matrix,size_matrix)
          energy_interaction=calculation_energy(1,new_i,new_j,lattice_cell,lattice_matrix,size_matrix)
          energy_agar=calculation_agar(lattice_agar,old_i,old_j)
          
          if(runif(1)<exp(-(c_weight*energy_capacity+j_weight*energy_interaction+n1_weight*energy_agar)))
          {
            #freq_prod
            freq_of_prod[new_i,new_j]=freq_of_prod[new_i,new_j]+1
            
            lattice_cell[new_i,new_j]=lattice_cell[new_i,new_j]+1
            lattice_cell_new[new_i,new_j]=lattice_cell_new[new_i,new_j]+1
            index_cell_i=c(index_cell_i,new_i)
            index_cell_j=c(index_cell_j,new_j)
            if(runif(1,0,1)<0.5)
              #age_cell[indexcell]=age_cell[indexcell]+1
            {age_cell=c(age_cell,0)}
            else
            {age_cell=c(age_cell,age_cell[indexcell])
            age_cell[indexcell]=0}
            live_or_death=c(live_or_death,0)
            total_cell=total_cell+1
            successunmber_cell=successunmber_cell+1
            lattice_agar[old_i,old_j]=lattice_agar[old_i,old_j]-2
          }
          
          
          # newenergy=calculation_energy(1,new_i,new_j,lattice_cell,lattice_matrix,size_matrix)
          # oldenergy=0
          # 
          # if(newenergy<=oldenergy)
          # {
          #   lattice_cell[new_i,new_j]=lattice_cell[new_i,new_j]+1
          #   lattice_cell_new[new_i,new_j]=lattice_cell_new[new_i,new_j]+1
          #   index_cell_i=c(index_cell_i,new_i)
          #   index_cell_j=c(index_cell_j,new_j)
          #   total_cell=total_cell+1
          #   successunmber_cell=successunmber_cell+1
          # }
          # else
          # {
          #   if(runif(1)<exp(-(newenergy-oldenergy))/exp(-(oldenergy-newenergy)))
          #   {
          #     lattice_cell[new_i,new_j]=lattice_cell[new_i,new_j]+1
          #     lattice_cell_new[new_i,new_j]=lattice_cell_new[new_i,new_j]+1
          #     index_cell_i=c(index_cell_i,new_i)
          #     index_cell_j=c(index_cell_j,new_j)
          #     total_cell=total_cell+1
          #     successunmber_cell=successunmber_cell+1
          #   }
          # }
          
          
          
          
          
        }
      }
    }
  }
  #secrete
  else if(twolayer<(division_rate+secrete_rate)/totalrate)
  {
    indexcell=sample(1:total_cell,size=1)
    old_i=index_cell_i[indexcell]
    old_j=index_cell_j[indexcell]
    
    new_ij=choose_newsite_create(old_i,old_j)
    new_i=new_ij[1]
    new_j=new_ij[2]
    
    if(new_j>0 & new_j<(size_matrix+1) & new_i>0 & new_i<(size_matrix+1) & live_or_death[indexcell]==0)
    {
      if(lattice_matrix[new_i,new_j]<(limit_of_matrix_layer) & lattice_agar[old_i,old_j]>0)
      {
        #energy
        energy_capacity=calculation_capacity(2,new_i,new_j,lattice_cell,lattice_matrix,size_matrix)
        energy_interaction=calculation_energy(2,new_i,new_j,lattice_cell,lattice_matrix,size_matrix)
        energy_agar=calculation_agar(lattice_agar,old_i,old_j)
        
        if(runif(1)<exp(-(c_weight*energy_capacity+j_weight*energy_interaction+n2_weight*energy_agar)))
        {
          #freq_prod
          freq_of_prod[new_i,new_j]=freq_of_prod[new_i,new_j]+1
          
          lattice_matrix[new_i,new_j]=lattice_matrix[new_i,new_j]+1
          lattice_matrix_new[new_i,new_j]=lattice_matrix_new[new_i,new_j]+1
          index_matrix_i=c(index_matrix_i,new_i)
          index_matrix_j=c(index_matrix_j,new_j)
          total_matrix=total_matrix+1
          successunmber_matrix=successunmber_matrix+1
          lattice_agar[old_i,old_j]=lattice_agar[old_i,old_j]-1
        }
        
        
        
        # newenergy=calculation_energy(2,new_i,new_j,lattice_cell,lattice_matrix,size_matrix)
        # oldenergy=0
        # 
        # if(newenergy<=oldenergy)
        # {
        #   lattice_matrix[new_i,new_j]=lattice_matrix[new_i,new_j]+1
        #   lattice_matrix_new[new_i,new_j]=lattice_matrix_new[new_i,new_j]+1
        #   index_matrix_i=c(index_matrix_i,new_i)
        #   index_matrix_j=c(index_matrix_j,new_j)
        #   total_matrix=total_matrix+1
        #   successunmber_matrix=successunmber_matrix+1
        # }
        # else
        # {
        #   if(runif(1)<exp(-(newenergy-oldenergy))/exp(-(oldenergy-newenergy)))
        #   {
        #     lattice_matrix[new_i,new_j]=lattice_matrix[new_i,new_j]+1
        #     lattice_matrix_new[new_i,new_j]=lattice_matrix_new[new_i,new_j]+1
        #     index_matrix_i=c(index_matrix_i,new_i)
        #     index_matrix_j=c(index_matrix_j,new_j)
        #     total_matrix=total_matrix+1
        #     successunmber_matrix=successunmber_matrix+1
        #   }
        # }
      }
    }
  }
  #drifting
  else
  {
    if(drift_cell_rate==0 & drift_matrix_rate==0)
    {
      indexall=sample(1:(total_cell+total_matrix),size=1)
    }
    else
    {
      driftif=runif(1)
      if(driftif<(drift_cell_rate/(drift_cell_rate+drift_matrix_rate*balancing_quantity_matrix+drift_agar_rate)))
      {
        indexall=sample(1:total_cell,size=1)
      }
      else if(driftif<((drift_cell_rate+drift_matrix_rate*balancing_quantity_matrix)/(drift_cell_rate+drift_matrix_rate*balancing_quantity_matrix+drift_agar_rate)))
      {
        indexall=total_cell+sample(1:total_matrix,size=1)
      }
      else
      {
        indexall=total_cell+total_matrix+1
      }
    }
    # drifting cell
    if(indexall<(total_cell+1))
    {
      indexcell=indexall
      
      
      if(live_or_death[indexcell]==0)
      {
        old_i=index_cell_i[indexcell]
        old_j=index_cell_j[indexcell]
        
        new_ij=choose_newsite_drift(old_i,old_j)
        new_i=new_ij[1]
        new_j=new_ij[2]
        
        if(new_j>0 & new_j<(size_matrix+1) & new_i>0 & new_i<(size_matrix+1))
        {
          if(lattice_cell[new_i,new_j]<(limit_of_cell_layer))
          {
            #energy
            lattice_cell_new[old_i,old_j]=lattice_cell_new[old_i,old_j]-1
            newenergy=c_weight*calculation_capacity(1,new_i,new_j,lattice_cell_new,lattice_matrix,size_matrix)+j_weight*calculation_energy(1,new_i,new_j,lattice_cell_new,lattice_matrix,size_matrix)
            oldenergy=c_weight*calculation_capacity(1,old_i,old_j,lattice_cell_new,lattice_matrix,size_matrix)+j_weight*calculation_energy(1,old_i,old_j,lattice_cell_new,lattice_matrix,size_matrix)
            
            if(newenergy<=oldenergy)
            {
              
              #freq_diff
              freq_of_diff[new_i,new_j]=freq_of_diff[new_i,new_j]+1
              
              
              lattice_cell[new_i,new_j]=lattice_cell[new_i,new_j]+1
              lattice_cell[old_i,old_j]=lattice_cell[old_i,old_j]-1
              lattice_cell_new[new_i,new_j]=lattice_cell_new[new_i,new_j]+1
              index_cell_i[indexcell]=new_i
              index_cell_j[indexcell]=new_j
              successunmber_cellmove=successunmber_cellmove+1
              if(live_or_death[indexcell]==1)
              {
                lattice_deathcell[new_i,new_j]=lattice_deathcell[new_i,new_j]+1
                lattice_deathcell[old_i,old_j]=lattice_deathcell[old_i,old_j]-1
              }
            }
            else
            {
              if(runif(1)<exp(-(newenergy-oldenergy)))
              {
                #freq_diff
                freq_of_diff[new_i,new_j]=freq_of_diff[new_i,new_j]+1
                
                lattice_cell[new_i,new_j]=lattice_cell[new_i,new_j]+1
                lattice_cell[old_i,old_j]=lattice_cell[old_i,old_j]-1
                lattice_cell_new[new_i,new_j]=lattice_cell_new[new_i,new_j]+1
                index_cell_i[indexcell]=new_i
                index_cell_j[indexcell]=new_j
                successunmber_cellmove=successunmber_cellmove+1
                if(live_or_death[indexcell]==1)
                {
                  lattice_deathcell[new_i,new_j]=lattice_deathcell[new_i,new_j]+1
                  lattice_deathcell[old_i,old_j]=lattice_deathcell[old_i,old_j]-1
                }
              }
              else
              {
                lattice_cell_new[old_i,old_j]=lattice_cell_new[old_i,old_j]+1
              }
            }
          }
        }
      }
    }
    #drifting matrix
    else if(indexall<(total_matrix+total_cell+1))
    {
      indexmatrix=indexall-total_cell
      old_i=index_matrix_i[indexmatrix]
      old_j=index_matrix_j[indexmatrix]
      
      new_ij=choose_newsite_drift(old_i,old_j)
      new_i=new_ij[1]
      new_j=new_ij[2]
      
      if(new_j>0 & new_j<(size_matrix+1) & new_i>0 & new_i<(size_matrix+1))
      {
        if(lattice_matrix[new_i,new_j]<(limit_of_matrix_layer))
        {
          #energy
          lattice_matrix_new[old_i,old_j]=lattice_matrix_new[old_i,old_j]-1
          
          newenergy=c_weight*calculation_capacity(2,new_i,new_j,lattice_cell,lattice_matrix_new,size_matrix)+j_weight*calculation_energy(2,new_i,new_j,lattice_cell,lattice_matrix_new,size_matrix)
          oldenergy=c_weight*calculation_capacity(2,old_i,old_j,lattice_cell,lattice_matrix_new,size_matrix)+j_weight*calculation_energy(2,old_i,old_j,lattice_cell,lattice_matrix_new,size_matrix)
          
          if(newenergy<=oldenergy)
          {
            #freq_diff
            freq_of_diff[new_i,new_j]=freq_of_diff[new_i,new_j]+1
            
            lattice_matrix[new_i,new_j]=lattice_matrix[new_i,new_j]+1
            lattice_matrix[old_i,old_j]=lattice_matrix[old_i,old_j]-1
            lattice_matrix_new[new_i,new_j]=lattice_matrix_new[new_i,new_j]+1
            index_matrix_i[indexmatrix]=new_i
            index_matrix_j[indexmatrix]=new_j
            successunmber_matrixmove=successunmber_matrixmove+1
          }
          else
          {
            if(runif(1)<exp(-(newenergy-oldenergy)))
            {
              #freq_diff
              freq_of_diff[new_i,new_j]=freq_of_diff[new_i,new_j]+1
              
              lattice_matrix[new_i,new_j]=lattice_matrix[new_i,new_j]+1
              lattice_matrix[old_i,old_j]=lattice_matrix[old_i,old_j]-1
              lattice_matrix_new[new_i,new_j]=lattice_matrix_new[new_i,new_j]+1
              index_matrix_i[indexmatrix]=new_i
              index_matrix_j[indexmatrix]=new_j
              successunmber_matrixmove=successunmber_matrixmove+1
            }
            else
            {
              lattice_matrix_new[old_i,old_j]=lattice_cell_new[old_i,old_j]+1
            }
          }
        }
      }
    }
    # drift agar
    else
    {
      
      if(steps>freeze_agar)
      {
        
        #(accelerate)
        
        for(repeatdrift in 1:(as.integer(max_agar*size_matrix*size_matrix/successunmber_cell)))
        {
          old_i=sample(1:(size_matrix),size=1)
          
          old_j=sample(1:(size_matrix),size=1)
          
          new_i=sample((old_i-1):(old_i+1),size=1)
          new_j=sample((old_j-1):(old_j+1),size=1)
          
          if(new_j>0 & new_j<(size_matrix+1) & new_i>0 & new_i<(size_matrix+1))
          {
            if(runif(1)<(lattice_agar[old_i,old_j]/max_agar) & lattice_agar[new_i,new_j]<max_agar)
            {
              lattice_agar[new_i,new_j]=lattice_agar[new_i,new_j]+1
              lattice_agar[old_i,old_j]=lattice_agar[old_i,old_j]-1
              
            }
          }
          
        }
        
      }
      
      
    }
    
    
    
    
    
    
    
    
  }
  
  
  age_cell=age_cell+1
  
  #counting for realtime
  realtime=realtime+basictimestep/(successunmber_cell*2+successunmber_matrix)
  
  
  
  
  
  #draw pic
  if(as.integer(steps/moviestep_biofilm)==(steps/moviestep_biofilm))
  {
    total_physical_time[(steps/moviestep_biofilm)]=realtime
    
    if((steps/moviestep_biofilm)>1)
    {
      if(cutting_time<(realtime) & total_physical_time[(steps/moviestep_biofilm)-1]<cutting_time)
      {
        ifcut=(steps/moviestep_biofilm)
      }
    }
    
    
    #cutline
    cutline_cell=rbind(cutline_cell,lattice_cell)
    cutline_matrix=rbind(cutline_matrix,lattice_matrix)
    

    

    
    TL_successunmber_cell=c(TL_successunmber_cell,successunmber_cell-sum(live_or_death))
    TL_successunmber_matrix=c(TL_successunmber_matrix,successunmber_matrix)
    TL_successunmber_cellmove=c(TL_successunmber_cellmove,successunmber_cellmove)
    TL_successunmber_matrixmove=c(TL_successunmber_matrixmove,successunmber_matrixmove)
    
    
    Total_death_cell=c(Total_death_cell,sum(live_or_death))
    if(ifcut==(steps/moviestep_biofilm))
    {
      
      #give the p of leaving
      
      
      
      ifleftcells=(sum(lattice_cell[(1):(50),51:100])-sum(lattice_deathcell[(1):(50),51:100]))*ifleftcells/(length(which((lattice_cell[(51):(100),1:50]+lattice_matrix[(51):(100),1:50])>0)))
      
      
      
      #cut a sqr
      
      if(ifsqr==1)
      {
        
        for(coorx in coorx1:coorx2)
        {
          for(coory in coory1:coory2)
          {
            lattice_cell[coorx,coory]=0
            lattice_cell_new[coorx,coory]=0
            lattice_deathcell[coorx,coory]=0
            lattice_matrix[coorx,coory]=0
            lattice_matrix_new[coorx,coory]=0
            indexsites=which(index_cell_i==coorx & index_cell_j==coory)
            indexsites_m=which(index_matrix_i==coorx & index_matrix_j==coory)
            if(length(indexsites)!=0)
            {
              age_cell=age_cell[-indexsites]
              index_cell_i=index_cell_i[-indexsites]
              index_cell_j=index_cell_j[-indexsites]
              live_or_death=live_or_death[-indexsites]
              total_cell=total_cell-length(indexsites)
              successunmber_cell=successunmber_cell-length(indexsites)
            }
            if(length(indexsites_m)!=0)
            {
              index_matrix_i=index_matrix_i[-indexsites_m]
              index_matrix_j=index_matrix_j[-indexsites_m]
              
              total_matrix=total_matrix-length(indexsites_m)
              successunmber_matrix=successunmber_matrix-length(indexsites_m)
            }
            
            
            
            
            if(runif(1)<ifleftcells)
            {
              if(length(indexsites)!=0 | length(indexsites_m)!=0)
              {
              lattice_cell[coorx,coory]=1
              lattice_cell_new[coorx,coory]=1
              
              age_cell=c(age_cell,1)
              index_cell_i=c(index_cell_i,coorx)
              index_cell_j=c(index_cell_j,coory)
              live_or_death=c(live_or_death,0)
              total_cell=total_cell+1
              successunmber_cell=successunmber_cell+1
              }
            }
          }
        }
        
        
        
        
        pdf(file='cutting_grey.pdf',width=9,height=9)
        
        max_celllayer=max(lattice_cell)
        max_matrixlayer=max(lattice_matrix)
        par(mar=c(0, 0, 0, 0))
        plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        #mtext(Nameofpic,side=1,line=0,cex=1)
        
        for(i in 1:size_matrix)
        {
          for(j in 1:size_matrix)
          {
            par(new=TRUE)
            if((lattice_matrix[i,j]+lattice_cell[i,j])!=0)
            {
              if((lattice_matrix[i,j]+lattice_cell[i,j])<10)
              {
                RoyalBlue=rgb((200-(lattice_matrix[i,j]+lattice_cell[i,j])*17)/255,(200-(lattice_matrix[i,j]+lattice_cell[i,j])*17)/255,(200-(lattice_matrix[i,j]+lattice_cell[i,j])*17)/255)
              }
              else
              {
                RoyalBlue=rgb((30)/255,(30)/255,(30)/255)
              }
              polygon(c(j-1,j,j,j-1),c(size_matrix-i,size_matrix-i,size_matrix-i+1,size_matrix-i+1), density = NULL, border = F, col = RoyalBlue)
              
            }
          }
        }
        box("plot",col="white",lty="solid")
        while (!is.null(dev.list()))  dev.off()
        
        
        pdf(file='cutting_biofilm.pdf',width=9,height=9)
        
        max_celllayer=max(lattice_cell)
        max_matrixlayer=max(lattice_matrix)
        par(mar=c(0, 0, 0, 0))
        plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        #mtext(Nameofpic,side=1,line=0,cex=1)
        
        
        for(i in 1:size_matrix)
        {
          for(j in 1:size_matrix)
          {
            par(new=TRUE)
            if(lattice_matrix[i,j]!=0)
            {
              if(lattice_matrix[i,j]<10)
              {
                RoyalBlue=rgb((200-lattice_matrix[i,j]*20)/255,(200-lattice_matrix[i,j]*20)/255,(255)/255)
              }
              else
              {
                RoyalBlue=rgb((0)/255,(0)/255,(255)/255)
              }
              polygon(c(j-1,j,j,j-1),c(size_matrix-i,size_matrix-i,size_matrix-i+1,size_matrix-i+1), density = NULL, border = F, col = RoyalBlue)
              
            }
          }
        }
        
        for(i in 1:size_matrix)
        {
          for(j in 1:size_matrix)
          {
            par(new=TRUE)
            if(lattice_cell[i,j]!=0)
            {
              if(lattice_cell[i,j]<10)
              {
                IndianRed=rgb((255)/255,(200-lattice_cell[i,j]*20)/255,(200-lattice_cell[i,j]*20)/255)
              }
              else
              {
                IndianRed=rgb((255)/255,(0)/255,(0)/255)
              }
              polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size_matrix-i+0.2,size_matrix-i+0.2,size_matrix-i+1-0.2,size_matrix-i+1-0.2), density = NULL, border = F, col = IndianRed)
            }
          }
        }
        
        
        par(new=TRUE)
        polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(12,12,10,10), density = NULL, border = F, col = 'grey')
        box("plot",col="white",lty="solid")
        while (!is.null(dev.list()))  dev.off()
        
        
        #par(new=TRUE)
        #plot(seq(1,size_matrix,1),rep(size_matrix-cutline1y,size_matrix),type="l",lwd=2,tck=0.03,cex=1,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        #par(new=TRUE)
        #plot(seq(1,size_matrix,1),rep(size_matrix-cutline2y,size_matrix),type="l",lwd=2,tck=0.03,cex=1,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        #par(new=TRUE)
        #plot(seq(1,size_matrix,1),rep(size_matrix-cutline3y,size_matrix),type="l",lwd=2,tck=0.03,cex=1,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        
        #text(x=1,y=size_matrix-cutline1y+5, "a",cex=3)
        #text(x=1,y=size_matrix-cutline2y+5, "b",cex=3)
        #text(x=1,y=size_matrix-cutline3y+5, "c",cex=3)
        
        
        
        
        
        pdf(file='cutting_agar.pdf',width=9,height=9)
        
        
        par(mar=c(0, 0, 0, 0))
        #mtext(Nameofpic,side=1,line=0,cex=1)
        
        
        plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        
        
        max_agar_current=max(lattice_agar)
        if(max_agar_current==0)
        {
          max_agar_current=1
        }
        for(i in 1:size_matrix)
        {
          for(j in 1:size_matrix)
          {
            par(new=TRUE)
            {
              polygon(c(j-1+0.1,j-0.1,j-0.1,j-1+0.1),c(size_matrix-i+0.1,size_matrix-i+0.1,size_matrix-i+1-0.1,size_matrix-i+1-0.1), density = NULL, border = F, col = rgb((130+115*(max_agar_current-lattice_agar[i,j])/max_agar_current)/255,(160+95*(max_agar_current-lattice_agar[i,j])/max_agar_current)/255,(0+255*(max_agar_current-lattice_agar[i,j])/max_agar_current)/255))
            }
          }
        }
        
        par(new=TRUE)
        polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(12,12,10,10), density = NULL, border = F, col = 'grey')
        box("plot",col="white",lty="solid")
        while (!is.null(dev.list()))  dev.off()
        
        
        
        
        
        pdf(file='cutting_inactive.pdf',width=9,height=9)
        
        
        par(mar=c(0, 0, 0, 0))
        plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
        
        
        max_deathcell=max(lattice_deathcell)
        if(max_deathcell==0)
        {
          max_deathcell=1
        }
        for(i in 1:size_matrix)
        {
          for(j in 1:size_matrix)
          {
            par(new=TRUE)
            if(lattice_cell[i,j]!=0)
            {
              number_deathcell=lattice_deathcell[i,j]
              if(number_deathcell!=0)
              {
                if(number_deathcell<10)
                {
                  Indiangrey=rgb((155-number_deathcell*15)/255,(156-number_deathcell*15)/255,(156-number_deathcell*15)/255)
                }
                else
                {
                  Indiangrey=rgb((155-150)/255,(156-150)/255,(156-150)/255)
                }
                polygon(c(j-1+0.1,j-0.1,j-0.1,j-1+0.1),c(size_matrix-i+0.1,size_matrix-i+0.1,size_matrix-i+1-0.1,size_matrix-i+1-0.1), density = NULL, border = F, col = Indiangrey)
              }
            }
          }
        }
        
        
        par(new=TRUE)
        polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(12,12,10,10), density = NULL, border = F, col = 'grey')
        box("plot",col="white",lty="solid")
        while (!is.null(dev.list()))  dev.off()
        
        write.table(lattice_cell,file='cutting_cell.csv',sep = ',',row.names = F,col.names = F)
        write.table(lattice_agar,file='cutting_agar.csv',sep = ',',row.names = F,col.names = F)
        write.table(lattice_matrix,file='cutting_matrix.csv',sep = ',',row.names = F,col.names = F)
        write.table(lattice_deathcell,file='cutting_death.csv',sep = ',',row.names = F,col.names = F)
        
        
        
        
      }
      
      #cut a round
      
      if(ifround==1)
      {
        coorxvec=c()
        cooryvec=c()
        for(coorx_i in 1:size_matrix)
        {
          for(coory_j in 1:size_matrix)
          {
            if((abs(coorx_i-roundcenter_x)^2+abs(coory_j-roundcenter_y)^2)^0.5<round_r)
            {
              coorxvec=c(coorxvec,coorx_i)
              cooryvec=c(cooryvec,coory_j)
            }
          }
        }
        
        for(cutsite in 1:length(coorxvec))
        {
          coorx=coorxvec[cutsite]
          coory=cooryvec[cutsite]
          lattice_cell[coorx,coory]=0
          lattice_cell_new[coorx,coory]=0
          lattice_deathcell[coorx,coory]=0
          lattice_matrix[coorx,coory]=0
          lattice_matrix_new[coorx,coory]=0
          indexsites=which(index_cell_i==coorx & index_cell_j==coory)
          indexsites_m=which(index_matrix_i==coorx & index_matrix_j==coory)
          if(length(indexsites)!=0)
          {
            age_cell=age_cell[-indexsites]
            index_cell_i=index_cell_i[-indexsites]
            index_cell_j=index_cell_j[-indexsites]
            live_or_death=live_or_death[-indexsites]
            total_cell=total_cell-length(indexsites)
            successunmber_cell=successunmber_cell-length(indexsites)
          }
          if(length(indexsites_m)!=0)
          {
            index_matrix_i=index_matrix_i[-indexsites_m]
            index_matrix_j=index_matrix_j[-indexsites_m]
            
            total_matrix=total_matrix-length(indexsites_m)
            successunmber_matrix=successunmber_matrix-length(indexsites_m)
          }
          
        }
        
        
      }
      
      if(ifputnewcell==1)
      {
        for(coorx in coorx1_newcell:coorx2_newcell)
        {
          for(coory in coory1_newcell:coory2_newcell)
          {
            lattice_cell[coorx,coory]=1
            lattice_cell_new[coorx,coory]=1
            
            age_cell=c(age_cell,1)
            index_cell_i=c(index_cell_i,coorx)
            index_cell_j=c(index_cell_j,coory)
            live_or_death=c(live_or_death,0)
            total_cell=total_cell+1
            successunmber_cell=successunmber_cell+1
            
          }
        }
        
      }
      
      
    }
    
    if(refrashagar==1 & ifcut==(steps/moviestep_biofilm))
    {
      lattice_agar=matrix(max_agar,nrow=size_matrix, ncol=size_matrix)
    }
    
    #cutwound round statistic
    
    
    
    
  }
  
  if(realtime>record_time*record_index)
  {
    print(floor(realtime))
    
    a=floor(realtime)
    b=10
    c=1
    while(a/b>=1){b=b*10
    c=c+1}
    numberofmovie=as.character(a)
    NAME=paste0(numberofmovie,".pdf",sep='')
    for(loopadd0 in 1:(6-c))
      NAME=paste0(0,NAME)
    Nameofpic=paste0('t=',floor(realtime),'h')
  }
  
  #record
  if(realtime>record_time*record_index)
  {
    record_index=record_index+1
    record_cell=rbind(record_cell,lattice_cell)
    record_matrix=rbind(record_matrix,lattice_matrix)
    record_death=rbind(record_death,lattice_deathcell)
    record_agar=rbind(record_agar,lattice_agar)
    
    record_freq_of_prod=rbind(record_freq_of_prod,freq_of_prod)
    record_freq_of_diff=rbind(record_freq_of_diff,freq_of_diff)
    
    
    #draw frame
    


      pdf(file=NAME,width=9,height=12)
      layout(matrix(c(1,1,1,1,2,3),3,2,byrow=TRUE), widths = c(1,1,1))
      
      
      
      max_celllayer=max(lattice_cell)
      max_matrixlayer=max(lattice_matrix)
      par(mar=c(0, 0, 0, 0))
      plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
      #mtext(Nameofpic,side=1,line=0,cex=1)
      
      
      
      
      for(i in 1:size_matrix)
      {
        for(j in 1:size_matrix)
        {
          par(new=TRUE)
          if(lattice_matrix[i,j]!=0)
          {
            if(lattice_matrix[i,j]<10)
            {
              RoyalBlue=rgb((200-lattice_matrix[i,j]*20)/255,(200-lattice_matrix[i,j]*20)/255,(255)/255)
            }
            else
            {
              RoyalBlue=rgb((0)/255,(0)/255,(255)/255)
            }
            polygon(c(j-1,j,j,j-1),c(size_matrix-i,size_matrix-i,size_matrix-i+1,size_matrix-i+1), density = NULL, border = F, col = RoyalBlue)
            
          }
        }
      }
      
      for(i in 1:size_matrix)
      {
        for(j in 1:size_matrix)
        {
          par(new=TRUE)
          if(lattice_cell[i,j]!=0)
          {
            if(lattice_cell[i,j]<10)
            {
              IndianRed=rgb((255)/255,(200-lattice_cell[i,j]*20)/255,(200-lattice_cell[i,j]*20)/255)
            }
            else
            {
              IndianRed=rgb((255)/255,(0)/255,(0)/255)
            }
            polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size_matrix-i+0.2,size_matrix-i+0.2,size_matrix-i+1-0.2,size_matrix-i+1-0.2), density = NULL, border = F, col = IndianRed)
          }
        }
      }
      
      
      #par(new=TRUE)
      #plot(seq(1,size_matrix,1),rep(size_matrix-cutline1y,size_matrix),type="l",lwd=2,tck=0.03,cex=1,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
      #par(new=TRUE)
      #plot(seq(1,size_matrix,1),rep(size_matrix-cutline2y,size_matrix),type="l",lwd=2,tck=0.03,cex=1,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
      #par(new=TRUE)
      #plot(seq(1,size_matrix,1),rep(size_matrix-cutline3y,size_matrix),type="l",lwd=2,tck=0.03,cex=1,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
      
      #text(x=1,y=size_matrix-cutline1y+5, "a",cex=3)
      #text(x=1,y=size_matrix-cutline2y+5, "b",cex=3)
      #text(x=1,y=size_matrix-cutline3y+5, "c",cex=3)
      polygon(c(0,20,20,0),c(size_matrix-5,size_matrix-5,size_matrix,size_matrix), density = NULL, border = F, col = 'white')
      
      text(x=10,y=size_matrix-3, Nameofpic,cex=4)
      
      
      par(new=TRUE)
      polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(12,12,10,10), density = NULL, border = F, col = 'grey')
      
      text(size_matrix/6*5,y=5, "5mm",cex=4)
      
      
      
      
      plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
      
      
      max_agar_current=max(lattice_agar)
      if(max_agar_current==0)
      {
        max_agar_current=1
      }
      for(i in 1:size_matrix)
      {
        for(j in 1:size_matrix)
        {
          par(new=TRUE)
          {
            polygon(c(j-1+0.1,j-0.1,j-0.1,j-1+0.1),c(size_matrix-i+0.1,size_matrix-i+0.1,size_matrix-i+1-0.1,size_matrix-i+1-0.1), density = NULL, border = F, col = rgb((130+115*(max_agar_current-lattice_agar[i,j])/max_agar_current)/255,(160+95*(max_agar_current-lattice_agar[i,j])/max_agar_current)/255,(0+255*(max_agar_current-lattice_agar[i,j])/max_agar_current)/255))
          }
        }
      }
      
      
      par(new=TRUE)
      polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(14,14,10,10), density = NULL, border = F, col = 'grey')
      text(size_matrix/6*5,y=5, "5mm",cex=4)
      
      
      plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
      
      
      max_deathcell=max(lattice_deathcell)
      if(max_deathcell==0)
      {
        max_deathcell=1
      }
      for(i in 1:size_matrix)
      {
        for(j in 1:size_matrix)
        {
          par(new=TRUE)
          if(lattice_cell[i,j]!=0)
          {
            number_deathcell=lattice_deathcell[i,j]
            if(number_deathcell!=0)
            {
              if(number_deathcell<10)
              {
                Indiangrey=rgb((155-number_deathcell*15)/255,(156-number_deathcell*15)/255,(156-number_deathcell*15)/255)
              }
              else
              {
                Indiangrey=rgb((155-150)/255,(156-150)/255,(156-150)/255)
              }
              polygon(c(j-1+0.1,j-0.1,j-0.1,j-1+0.1),c(size_matrix-i+0.1,size_matrix-i+0.1,size_matrix-i+1-0.1,size_matrix-i+1-0.1), density = NULL, border = F, col = Indiangrey)
            }
          }
        }
      }
      
      par(new=TRUE)
      polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(14,14,10,10), density = NULL, border = F, col = 'grey')
      text(size_matrix/6*5,y=5, "5mm",cex=4)
      
      while (!is.null(dev.list()))  dev.off()
    
  }
  
  
  
  
}


while (!is.null(dev.list()))  dev.off()

#end simu





#record and write table




write.table(TL_successunmber_cell,file='cell.csv',sep = ',',row.names = F,col.names = F)
write.table(TL_successunmber_matrix,file='matrix.csv',sep = ',',row.names = F,col.names = F)
write.table(Total_death_cell,file='death.csv',sep = ',',row.names = F,col.names = F)


write.table(record_cell,file='record_cell.csv',sep = ',',row.names = F,col.names = F)
write.table(record_matrix,file='record_matrix.csv',sep = ',',row.names = F,col.names = F)


write.table(record_death,file='record_death.csv',sep = ',',row.names = F,col.names = F)
write.table(record_agar,file='record_agar.csv',sep = ',',row.names = F,col.names = F)

write.table(record_freq_of_prod,file='record_freq_of_prod.csv',sep = ',',row.names = F,col.names = F)
write.table(record_freq_of_diff,file='record_freq_of_diff.csv',sep = ',',row.names = F,col.names = F)


if(cutting_time<floor(max(total_physical_time)))
{
  construct_timepoint=seq(cutting_time,floor(max(total_physical_time)),3)
  real_frame_intime=seq(1,length(construct_timepoint),1)
  index_frame=1
  for(real_frame_intime_test in 2:length(total_physical_time))
  {
    if(total_physical_time[real_frame_intime_test]>construct_timepoint[index_frame] & total_physical_time[real_frame_intime_test-1]<construct_timepoint[index_frame])
    {
      real_frame_intime[index_frame]=real_frame_intime_test
      index_frame=index_frame+1
    }
    if(index_frame>length(construct_timepoint))
      break
  }
  real_frame_intime[1]=real_frame_intime[1]+1
  
  write.table(real_frame_intime,file='real_frame_intime.csv',sep = ',',row.names = F,col.names = F)
  write.table(construct_timepoint,file='wconstruct_timepoint.csv',sep = ',',row.names = F,col.names = F)
}



# circle


}
