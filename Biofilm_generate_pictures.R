
#set color
IndianRed=rgb(255/255,80/255,80/255)
RoyalBlue=rgb(65/255,105/255,200/255)

dirsave=paste0('path/S=1')
setwd(dirsave) 

too=73

record_cell_batch=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
record_matrix_batch=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
record_death_batch=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
record_agar_batch=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")

record_freq_of_prod_batch=read.table ('record_freq_of_prod.csv',header=F,check.names=FALSE,sep=",")
record_freq_of_diff_batch=read.table ('record_freq_of_diff.csv',header=F,check.names=FALSE,sep=",")

#cutting_cell_batch=read.table ('cutting_cell.csv',header=F,check.names=FALSE,sep=",")
#cutting_death_batch=read.table ('cutting_death.csv',header=F,check.names=FALSE,sep=",")
#cutting_matrix_batch=read.table ('cutting_matrix.csv',header=F,check.names=FALSE,sep=",")
#cutting_agar_batch=read.table ('cutting_agar.csv',header=F,check.names=FALSE,sep=",")


record_cell_batch=record_cell_batch[1:(1+too*100),]
record_matrix_batch=record_matrix_batch[1:(1+too*100),]
record_death_batch=record_death_batch[1:(1+too*100),]
record_agar_batch=record_agar_batch[1:(1+too*100),]

record_freq_of_prod_batch=record_freq_of_prod_batch[1:(1+too*100),]
record_freq_of_diff_batch=record_freq_of_diff_batch[1:(1+too*100),]




record_cell=record_cell_batch
record_matrix=record_matrix_batch
record_death=record_death_batch
record_agar=record_agar_batch

startfrom=1
total_h=72

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-98):(i*100+1),1:100]))
}

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-98):(i*100+1),1:100]))
}

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-98):(i*100+1),1:100]))
}




record_freq_of_prod=record_freq_of_prod_batch
record_freq_of_diff=record_freq_of_diff_batch


#cutting_cell=cutting_cell_batch
#cutting_death=cutting_death_batch
#cutting_matrix=cutting_matrix_batch
#cutting_agar=cutting_agar_batch

dir='path'
setwd(dir) 
all_file=list.files()
for(read in 2:5)
{
dirsave=paste0('path/',all_file[read])
setwd(dirsave) 


record_cell_batch=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
record_matrix_batch=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
record_death_batch=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
record_agar_batch=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")



record_freq_of_prod_batch=read.table ('record_freq_of_prod.csv',header=F,check.names=FALSE,sep=",")
record_freq_of_diff_batch=read.table ('record_freq_of_diff.csv',header=F,check.names=FALSE,sep=",")

#cutting_cell_batch=read.table ('cutting_cell.csv',header=F,check.names=FALSE,sep=",")
#cutting_death_batch=read.table ('cutting_death.csv',header=F,check.names=FALSE,sep=",")
#cutting_matrix_batch=read.table ('cutting_matrix.csv',header=F,check.names=FALSE,sep=",")
#cutting_agar_batch=read.table ('cutting_agar.csv',header=F,check.names=FALSE,sep=",")


record_cell_batch=record_cell_batch[1:(1+too*100),]
record_matrix_batch=record_matrix_batch[1:(1+too*100),]
record_death_batch=record_death_batch[1:(1+too*100),]
record_agar_batch=record_agar_batch[1:(1+too*100),]

record_freq_of_prod_batch=record_freq_of_prod_batch[1:(1+too*100),]
record_freq_of_diff_batch=record_freq_of_diff_batch[1:(1+too*100),]


record_cell=record_cell+record_cell_batch
record_matrix=record_matrix+record_matrix_batch
record_death=record_death+record_death_batch
record_agar=record_agar+record_agar_batch

startfrom=1
total_h=72

TL_successunmber_cell_batch=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell_batch=c(TL_successunmber_cell_batch,sum(record_cell_batch[(i*100-98):(i*100+1),1:100]))
}

TL_successunmber_matrix_batch=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix_batch=c(TL_successunmber_matrix_batch,sum(record_matrix_batch[(i*100-98):(i*100+1),1:100]))
}

Total_death_cell_batch=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell_batch=c(Total_death_cell_batch,sum(record_death_batch[(i*100-98):(i*100+1),1:100]))
}


TL_successunmber_cell=rbind(TL_successunmber_cell,TL_successunmber_cell_batch)
TL_successunmber_matrix=rbind(TL_successunmber_matrix,TL_successunmber_matrix_batch)
Total_death_cell=rbind(Total_death_cell,Total_death_cell_batch)

record_freq_of_prod=record_freq_of_prod+record_freq_of_prod_batch
record_freq_of_diff=record_freq_of_diff+record_freq_of_diff_batch

#cutting_cell=cutting_cell+cutting_cell_batch
#cutting_death=cutting_death+cutting_death_batch
#cutting_matrix=cutting_matrix+cutting_matrix_batch
#cutting_agar=cutting_agar+cutting_agar_batch




}

record_cell=record_cell/5
record_matrix=record_matrix/5
record_death=record_death/5
record_agar=record_agar/5

#TL_successunmber_cell=TL_successunmber_cell/5
#TL_successunmber_matrix=TL_successunmber_matrix/5
#Total_death_cell=Total_death_cell/5

#cutting_cell=cutting_cell/5
#cutting_death=cutting_death/5
#cutting_matrix=cutting_matrix/5
#cutting_agar=cutting_agar/5


TL_successunmber_cell=as.matrix(TL_successunmber_cell)
TL_successunmber_matrix=as.matrix(TL_successunmber_matrix)
Total_death_cell=as.matrix(Total_death_cell)

#separate figs

dir='path'
setwd(dir)

movieperh=35

lattice_cell=record_cell[(2+100*movieperh):(101+100*movieperh),]
lattice_matrix=record_matrix[(2+100*movieperh):(101+100*movieperh),]
lattice_agar=record_agar[(2+100*movieperh):(101+100*movieperh),]
lattice_deathcell=record_death[(2+100*movieperh):(101+100*movieperh),]


pdf(file=paste0('t=',movieperh+1,'grey.pdf'),width=9,height=9)

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


pdf(file=paste0('t=',movieperh+1,'biofilm.pdf'),width=9,height=9)

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





pdf(file=paste0('t=',movieperh+1,'agar.pdf'),width=9,height=9)


par(mar=c(0, 0, 0, 0))
#mtext(Nameofpic,side=1,line=0,cex=1)


plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")


max_agar_current=3

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





pdf(file=paste0('t=',movieperh+1,'inactive.pdf'),width=9,height=9)


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

#increase

#production includes 2h

pdf(file='heat_produc.pdf',width=12,height=12)
par(mar=c(1,1,1,1))
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")

freq_of_prod=record_freq_of_prod[4802:4901,]-record_freq_of_prod[4602:4701,]
freq_of_prod=freq_of_prod/max(freq_of_prod)

for(i in 1:size_matrix)
{
  for(j in 1:size_matrix)
  {
    par(new=TRUE)
    if(freq_of_prod[i,j]!=0)
    {
      
      RoyalBlue=rgb((220-freq_of_prod[i,j]*150)/255,(150-freq_of_prod[i,j]*150)/255,(225-freq_of_prod[i,j]*150)/255)
      
      polygon(c(j-1,j,j,j-1),c(size_matrix-i,size_matrix-i,size_matrix-i+1,size_matrix-i+1), density = NULL, border = F, col = RoyalBlue)
      
    }
  }
}



par(new=TRUE)
polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(12,12,10,10), density = NULL, border = F, col = 'grey')
text(size_matrix/6*5,y=5, "",cex=4)
while (!is.null(dev.list()))  dev.off()


#diffusion includes 1h

pdf(file='heat_diff.pdf',width=12,height=12)
par(mar=c(1,1,1,1))
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")

freq_of_diff=record_freq_of_diff[4802:4901,]-record_freq_of_diff[4702:4801,]
freq_of_diff=freq_of_diff/max(freq_of_diff)

for(i in 1:size_matrix)
{
  for(j in 1:size_matrix)
  {
    par(new=TRUE)
    if(freq_of_diff[i,j]!=0)
    {
      
      RoyalBlue=rgb((220-freq_of_diff[i,j]*150)/255,(150-freq_of_diff[i,j]*150)/255,(225-freq_of_diff[i,j]*150)/255)
      
      polygon(c(j-1,j,j,j-1),c(size_matrix-i,size_matrix-i,size_matrix-i+1,size_matrix-i+1), density = NULL, border = F, col = RoyalBlue)
      
    }
  }
}



par(new=TRUE)
polygon(c(size_matrix/3*2,size_matrix,size_matrix,size_matrix/3*2),c(12,12,10,10), density = NULL, border = F, col = 'grey')
text(size_matrix/6*5,y=5, "",cex=4)

while (!is.null(dev.list()))  dev.off()





#show the hist diagram of cross section

cells_48h=record_cell[4702:4801,]
matrix_48h=record_matrix[4702:4801,]
Inactive_48h=record_death[4702:4801,]
cells_48h=cells_48h-Inactive_48h
biomass_48h=cells_48h+Inactive_48h+matrix_48h

IndianRed=rgb(255/255,80/255,80/255)
RoyalBlue=rgb(65/255,105/255,200/255)

pdf(file='hist.pdf',width=6,height=4)

par(mar=c(2, 2.2, 0.5, 0.5))
plot(c(seq(1,100,1),1),c(colSums(biomass_48h[45:55,])/11,0),type="p",tck=0.01,cex=0.01,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,100),ylim=c(0,10),bty="o",xaxt="n",yaxt="n")

polygon(c(seq(1,100,1),1),c(colSums(biomass_48h[45:55,])/11,0),type="p",tck=0.01,cex=1,las=1,xlab="",col=RoyalBlue,pch=19, ylab="", main="",xlim=c(0,100),ylim=c(0,10),bty="l",xaxt="n",yaxt="n",border = F)

par(new=T)

polygon(c(seq(1,100,1),1),c(colSums(biomass_48h[45:55,])/11-colSums(matrix_48h[45:55,])/11,0),type="p",tck=0.01,cex=1,las=1,xlab="",col=IndianRed,pch=19, ylab="", main="",xlim=c(0,100),ylim=c(0,10),bty="l",xaxt="n",yaxt="n",border = F)

par(new=T)

polygon(c(seq(1,100,1),1),c(colSums(Inactive_48h[45:55,])/11,0),type="p",tck=0.01,cex=1,las=1,xlab="",col='grey',pch=19, ylab="", main="",xlim=c(0,100),ylim=c(0,10),xaxt="n",yaxt="n",border = F)

axis(side=1,las=1,at=seq(0,100,20),mgp=c(0,0.5,0),tck=0.02,labels=seq(0,15,3),cex.axis=2.1)

axis(side=2,las=1,at=seq(0,10,5),mgp=c(0,0.5,0),tck=0.02,las=1,labels=seq(0,10,5),cex.axis=2.1)


while (!is.null(dev.list()))  dev.off()



#dynamic

IndianRed=rgb(255/255,80/255,80/255)
RoyalBlue=rgb(65/255,105/255,200/255)

pdf(file='dynamic.pdf',width=6,height=4)

startfrom=1
total_h=72

TL_successunmber_cell_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell_sd=c(TL_successunmber_cell_sd,sd(TL_successunmber_cell[,i]))
}

TL_successunmber_matrix_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix_sd=c(TL_successunmber_matrix_sd,sd(TL_successunmber_matrix[,i]))
}

Total_death_cell_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell_sd=c(Total_death_cell_sd,sd(Total_death_cell[,i]))
}






ylim_dynamic=1.2*(max(Total_death_cell+TL_successunmber_matrix+TL_successunmber_cell))

par(mar=c(2,2,1,1))

#std
plot(c(0,seq(startfrom,total_h,1)),c(21,colSums(TL_successunmber_cell[,1:72])/5),type="l",lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),yaxt="n",bty="L")
par(new=TRUE)

polygon(c(0,seq(startfrom,total_h,1),seq(total_h,startfrom,-1),0),c(21,colSums(TL_successunmber_cell[,1:72]-Total_death_cell[,1:72])/5-TL_successunmber_cell_sd,colSums(TL_successunmber_cell[,72:1]-Total_death_cell[,72:1])/5+TL_successunmber_cell_sd[72:1],21), density = NULL, border = F, col = 'pink')

par(new=TRUE)

polygon(c(0,seq(startfrom,total_h,1),seq(total_h,startfrom,-1),0),c(0,colSums(TL_successunmber_matrix[,1:72])/5-TL_successunmber_matrix_sd,colSums(TL_successunmber_matrix[,72:1])/5+TL_successunmber_matrix_sd[72:1],0), density = NULL, border = F, col = 'lightblue')

par(new=TRUE)

polygon(c(0,seq(startfrom,total_h,1),seq(total_h,startfrom,-1),0),c(0,colSums(Total_death_cell[,1:72])/5-Total_death_cell_sd,colSums(Total_death_cell[,72:1])/5+Total_death_cell_sd[72:1],0), density = NULL, border = F, col = 'grey')

par(new=TRUE)

polygon(c(0,seq(startfrom,total_h,1),seq(total_h,startfrom,-1),0),c(21,colSums(TL_successunmber_cell[,1:72]-Total_death_cell[,1:72]+TL_successunmber_matrix[,1:72])/5-TL_successunmber_cell_sd-TL_successunmber_matrix_sd,colSums(TL_successunmber_cell[,72:1]-Total_death_cell[,72:1]+TL_successunmber_matrix[,72:1])/5+TL_successunmber_cell_sd[72:1]+TL_successunmber_matrix_sd[72:1],21), density = NULL, border = F, col = 'lightgreen')

par(new=TRUE)

#mean
plot(c(0,seq(startfrom,total_h,1)),c(0,colSums(TL_successunmber_matrix[,1:72])/5),type="l",lwd=1,tck=0.01,cex=0.3,las=1,xlab="",col='RoyalBlue',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),xaxt="n",yaxt="n",bty="L")
par(new=TRUE)
plot(c(0,seq(startfrom,total_h,1)),c(0,colSums(Total_death_cell[,1:72])/5),type="l",lwd=1,tck=0.01,cex=0.3,las=1,xlab="",col='darkgrey',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),xaxt="n",yaxt="n",bty="L")
par(new=TRUE)
plot(c(0,seq(startfrom,total_h,1)),c(0,colSums(-Total_death_cell[,1:72]+TL_successunmber_matrix[,1:72]+TL_successunmber_cell[,1:72])/5),type="l",lwd=1,tck=0.03,cex=0.3,las=1,xlab="",col='darkgreen',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),xaxt="n",yaxt='n',bty="L")
par(new=TRUE)
plot(c(0,seq(startfrom,total_h,1)),c(21,colSums(TL_successunmber_cell[,1:72]-Total_death_cell[,1:72])/5),type="l",lwd=1,tck=0.03,cex=0.3,las=1,xlab="",col='IndianRed',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),xaxt="n",yaxt='n',bty="L")
par(new=TRUE)
#plot(seq(1,countsteps_biofilm,1),TL_successunmber_cellmove,type="p",tck=0.03,cex=0.3,las=1,xlab="",col='pink',pch=19, ylab="", main="",xlim=c(0,countsteps_biofilm),ylim=c(0,50000),xaxt="n",yaxt="n",bty="L")
#par(new=TRUE)
#plot(seq(1,countsteps_biofilm,1),TL_successunmber_matrixmove,type="p",tck=0.03,cex=0.3,las=1,xlab="",col='cyan',pch=19, ylab="", main="",xlim=c(0,countsteps_biofilm),ylim=c(0,50000),xaxt="n",yaxt="n",bty="L")
axis(side=2,las=1,at=seq(0,25000,5000),mgp=c(0,0.5,0),tck=0.01,las=1,labels=seq(0,25000,5000)/1000,cex.axis=1)

#mtext(expression(~~~~~~~~~~~~~~~~~~~~~~~~(~~10^3)),side=2,line=2,cex=1.5)
while (!is.null(dev.list()))  dev.off()




#major

##error bar

pdf(file='bar.pdf',width=6,height=4)

plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,10),ylim=c(0,10),xaxt="n",yaxt="n",bty="L")
#mtext(Nameofpic,side=1,line=0,cex=1)

j=1
for(i in seq(0,10,0.05))
{
    par(new=TRUE)

        BarBlue=rgb((200-i*20)/255,(200-i*20)/255,(255)/255)

      polygon(c(j-1,j,j,j-1),c(i,i,i+1,i+1), density = NULL, border = F, col = BarBlue)

}

j=3
for(i in seq(0,10,0.05))
{
  par(new=TRUE)
  
  BarBlue=rgb((255)/255,(200-i*20)/255,(200-i*20)/255)
  
  polygon(c(j-1,j,j,j-1),c(i,i,i+1,i+1), density = NULL, border = F, col = BarBlue)
  
}


j=5
for(i in seq(0,10,0.05))
{
  par(new=TRUE)
  
  BarBlue=rgb((220-i/10*150)/255,(150-i/10*150)/255,(225-i/10*150)/255)
  
  polygon(c(j-1,j,j,j-1),c(i,i,i+1,i+1), density = NULL, border = F, col = BarBlue)
  
}

j=7
for(i in seq(0,10,0.05))
{
  par(new=TRUE)
  
  BarBlue=rgb((155-i*15)/255,(156-i*15)/255,(156-i*15)/255)
  
  polygon(c(j-1,j,j,j-1),c(i,i,i+1,i+1), density = NULL, border = F, col = BarBlue)
  
}

j=9

i=0

  par(new=TRUE)
  
  BarBlue=rgb((130+115*(3-i*0.3)/3)/255,(160+95*(3-i*0.3)/3)/255,(0+255*(3-i*0.3)/3)/255)

  
  polygon(c(j-1,j,j,j-1),c(i,i,i+11/4,i+11/4), density = NULL, border = F, col = BarBlue)
  
  i=10/3
  
  par(new=TRUE)
  
  BarBlue=rgb((130+115*(3-i*0.3)/3)/255,(160+95*(3-i*0.3)/3)/255,(0+255*(3-i*0.3)/3)/255)
  
  
  polygon(c(j-1,j,j,j-1),c(11/4,11/4,11/4+11/4,11/4+11/4), density = NULL, border = F, col = BarBlue)
  
  i=10/3*2
  
  par(new=TRUE)
  
  BarBlue=rgb((130+115*(3-i*0.3)/3)/255,(160+95*(3-i*0.3)/3)/255,(0+255*(3-i*0.3)/3)/255)
  
  
  polygon(c(j-1,j,j,j-1),c(11/4*2,11/4*2,11/4+11/4*2,11/4+11/4*2), density = NULL, border = F, col = BarBlue)
  
  
  i=10/3*3
  
  par(new=TRUE)
  
  BarBlue=rgb((130+115*(3-i*0.3)/3)/255,(160+95*(3-i*0.3)/3)/255,(0+255*(3-i*0.3)/3)/255)
  
  
  polygon(c(j-1,j,j,j-1),c(11/4*3,11/4*3,11/4+11/4*3,11/4+11/4*3), density = NULL, border = F, col = BarBlue)
  
  

while (!is.null(dev.list()))  dev.off()



#OD with crossover

dirsave=paste0('path/S=1')
setwd(dirsave) 

too=73

record_cell_batch=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
record_matrix_batch=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
record_death_batch=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
record_agar_batch=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")


#cutting_cell_batch=read.table ('cutting_cell.csv',header=F,check.names=FALSE,sep=",")
#cutting_death_batch=read.table ('cutting_death.csv',header=F,check.names=FALSE,sep=",")
#cutting_matrix_batch=read.table ('cutting_matrix.csv',header=F,check.names=FALSE,sep=",")
#cutting_agar_batch=read.table ('cutting_agar.csv',header=F,check.names=FALSE,sep=",")


record_cell_batch=record_cell_batch[1:(1+too*100),]
record_matrix_batch=record_matrix_batch[1:(1+too*100),]
record_death_batch=record_death_batch[1:(1+too*100),]
record_agar_batch=record_agar_batch[1:(1+too*100),]


record_cell=record_cell_batch
record_matrix=record_matrix_batch
record_death=record_death_batch
record_agar=record_agar_batch

startfrom=25
total_h=72

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-98):(i*100-49),51:100]))
}

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-98):(i*100-49),51:100]))
}

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-98):(i*100-49),51:100]))
}


dir='path'
setwd(dir) 
all_file=list.files()
for(read in 2:5)
{
  dirsave=paste0('path/',all_file[read])
  setwd(dirsave) 
  
  record_cell_batch=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
  record_matrix_batch=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
  record_death_batch=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
  record_agar_batch=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")

  record_cell_batch=record_cell_batch[1:(1+too*100),]
  record_matrix_batch=record_matrix_batch[1:(1+too*100),]
  record_death_batch=record_death_batch[1:(1+too*100),]
  record_agar_batch=record_agar_batch[1:(1+too*100),]
  
  record_cell=record_cell+record_cell_batch
  record_matrix=record_matrix+record_matrix_batch
  record_death=record_death+record_death_batch
  record_agar=record_agar+record_agar_batch
  
  startfrom=25
  total_h=72
  
  TL_successunmber_cell_batch=vector()
  for(i in seq(startfrom,total_h,1))
  {
    TL_successunmber_cell_batch=c(TL_successunmber_cell_batch,sum(record_cell_batch[(i*100-98):(i*100-49),51:100]))
  }
  
  TL_successunmber_matrix_batch=vector()
  for(i in seq(startfrom,total_h,1))
  {
    TL_successunmber_matrix_batch=c(TL_successunmber_matrix_batch,sum(record_matrix_batch[(i*100-98):(i*100-49),51:100]))
  }
  
  Total_death_cell_batch=vector()
  for(i in seq(startfrom,total_h,1))
  {
    Total_death_cell_batch=c(Total_death_cell_batch,sum(record_death_batch[(i*100-98):(i*100-49),51:100]))
  }
  
  
  TL_successunmber_cell=rbind(TL_successunmber_cell,TL_successunmber_cell_batch)
  TL_successunmber_matrix=rbind(TL_successunmber_matrix,TL_successunmber_matrix_batch)
  Total_death_cell=rbind(Total_death_cell,Total_death_cell_batch)
  
  
  
}

record_cell=record_cell/5
record_matrix=record_matrix/5
record_death=record_death/5
record_agar=record_agar/5


TL_successunmber_cell=as.matrix(TL_successunmber_cell)
TL_successunmber_matrix=as.matrix(TL_successunmber_matrix)
Total_death_cell=as.matrix(Total_death_cell)

#cal of normal with ratio

startfrom=1
total_h=48

TL_successunmber_cell=TL_successunmber_matrix/(TL_successunmber_cell-Total_death_cell)

TL_successunmber_cell_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell_sd=c(TL_successunmber_cell_sd,sd(TL_successunmber_cell[,i]))
}

TL_successunmber_matrix_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix_sd=c(TL_successunmber_matrix_sd,sd(TL_successunmber_matrix[,i]))
}

Total_death_cell_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell_sd=c(Total_death_cell_sd,sd(Total_death_cell[,i]))
}







startfrom=1
total_h=48

TL_successunmber_cell_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell_sd=c(TL_successunmber_cell_sd,sd(TL_successunmber_cell[,i]))
}

TL_successunmber_matrix_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix_sd=c(TL_successunmber_matrix_sd,sd(TL_successunmber_matrix[,i]))
}

Total_death_cell_sd=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell_sd=c(Total_death_cell_sd,sd(Total_death_cell[,i]))
}


x_normal=c(12,24,36,48,60,72)
y_normal=c(0.35,0.81,1.66,1.56,2.01,1.74)
y_normal_std=c(0.12,0.12,0.03,0.03,0.04,0.04)
y_heal=c(0.38,1.41,1.72,1.85,1.81,1.81)
y_heal_std=c(0.105,0.105,0.03,0.03,0.1,0.1)

x_normal_1=c(16,24,40,48,64,72)
y_normal_1=c(0.201,0.862,1.839,1.946,2.089,1.975)
y_normal_1_std=c(0.040,0.096,0.084,0.045,0.048,0.058)
y_heal_1=c(1.094,1.405,1.776,1.836,1.972,1.992)
y_heal_1_std=c(0.071,0.145,0.077,0.074,0.027,0.101)

simu_normal=c(21,colSums(TL_successunmber_cell[,1:72]-Total_death_cell[,1:72])/5)
simu_normal_x=c(0,seq(startfrom,total_h,1))
simu_normal_std=TL_successunmber_cell_sd-Total_death_cell_sd

simu_heal=c(60,colSums(TL_successunmber_cell-Total_death_cell)/5)
simu_heal_x=seq(0,48,1)
simu_heal_std=c(10,TL_successunmber_cell_sd-Total_death_cell_sd)

scale_factor=1

dir='path'
setwd(dir) 


pdf(file='crossover.pdf',width=6,height=5)
par(mar=c(2,2.5,1,1))
  
plot(x_normal,y_normal,xlim=c(0,75),ylim=c(0,1300),pch=18,cex=0.9,col='white',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="o")
par(new=T)
polygon(c(simu_normal_x,c(seq(72,1,-1),0)),c(simu_normal-c(0,simu_normal_std),simu_normal[73:1]+c(simu_normal_std[72:1],0))*scale_factor/4, density = NULL, border = F, col = rgb(65/255,155/255,200/255,alpha=0.5))
par(new=T)
polygon(c(simu_heal_x,simu_heal_x[49:1]),c(simu_heal-simu_heal_std,(simu_heal+simu_heal_std)[49:1])*scale_factor, density = NULL, border = F, col = rgb(255/255,120/255,120/255,alpha=0.5))
par(new=T)
plot(seq(0,48,1),simu_heal*scale_factor,xlim=c(0,75),ylim=c(0,1300),type="l",lty=1,lwd=1,cex=0.9,col='IndianRed',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="o")
par(new=T)
plot(seq(0,72,1),simu_normal*scale_factor/4,xlim=c(0,75),ylim=c(0,1300),type="l",lty=1,lwd=1,col='RoyalBlue',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="o")
axis(side=2,las=1,at=seq(100,1300,300),mgp=c(0,0.5,0),tck=-0.015,las=1,labels=seq(0.1,1.3,0.3),cex.axis=1.5)
axis(side=1,las=1,at=seq(0,70,10),mgp=c(0,0.5,0),tck=-0.015,las=1,labels=seq(0,70,10),cex.axis=1.5)



par(new=T)
plot(x_normal,y_normal*1200/2,xlim=c(0,75),ylim=c(0,1300),pch=17,cex=1.5,col='IndianRed',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="L")
arrows(x_normal,(y_normal-y_normal_std)*1200/2,x_normal,(y_normal+y_normal_std)*1200/2,length=0.05,angle=90,col='IndianRed',lwd=1.5)
arrows(x_normal,(y_normal+y_normal_std)*1200/2,x_normal,(y_normal-y_normal_std)*1200/2,length=0.05,angle=90,col='IndianRed',lwd=1.5)
par(new=T)
plot(x_normal,y_heal*1200/2,xlim=c(0,75),ylim=c(0,1300),pch=2,cex=1.5,col='IndianRed',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="L")
arrows(x_normal,(y_heal-y_heal_std)*1200/2,x_normal,(y_heal+y_heal_std)*1200/2,length=0.05,angle=90,col='IndianRed',lwd=1.5)
arrows(x_normal,(y_heal+y_heal_std)*1200/2,x_normal,(y_heal-y_heal_std)*1200/2,length=0.05,angle=90,col='IndianRed',lwd=1.5)


par(new=T)
plot(x_normal_1,y_normal_1*1200/2,xlim=c(0,75),ylim=c(0,1300),pch=15,cex=1.5,col='RoyalBlue',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="L")
arrows(x_normal_1,(y_normal_1-y_normal_1_std)*1200/2,x_normal_1,(y_normal_1+y_normal_1_std)*1200/2,length=0.05,angle=90,col='RoyalBlue',lwd=1.5)
arrows(x_normal_1,(y_normal_1+y_normal_1_std)*1200/2,x_normal_1,(y_normal_1-y_normal_1_std)*1200/2,length=0.05,angle=90,col='RoyalBlue',lwd=1.5)
par(new=T)
plot(x_normal_1,y_heal_1*1200/2,xlim=c(0,75),ylim=c(0,1300),pch=0,cex=1.5,col='RoyalBlue',xlab='',ylab='',tck=0.01, main="",xaxt="n",yaxt="n",bty="L")
arrows(x_normal_1,(y_heal_1-y_heal_1_std)*1200/2,x_normal_1,(y_heal_1+y_heal_1_std)*1200/2,length=0.05,angle=90,col='RoyalBlue',lwd=1.5)
arrows(x_normal_1,(y_heal_1+y_heal_1_std)*1200/2,x_normal_1,(y_heal_1-y_heal_1_std)*1200/2,length=0.05,angle=90,col='RoyalBlue',lwd=1.5)

legend(0,1200,legend=c(''),cex=1.5,x.intersp=0.5,y.intersp=0,col=rgb(65/255,155/255,200/255,alpha=0.5),bty="n",pch=15,lwd=,text.col='white')
par(new=T)
legend(0,1100,legend=c(''),cex=1.5,x.intersp=0.5,y.intersp=0,col=rgb(255/255,120/255,120/255,alpha=0.5),bty="n",pch=15,lwd=,text.col='white')

while (!is.null(dev.list()))  dev.off()



#frames


#create movie

dir='path'
setwd(dir) 



for(movieperh in 0:72)
{
  

  
  NAME=paste0('t=',(movieperh+1)*1,'h','.pdf')
  
  Nameofpic=paste0('t=',(movieperh+1)*1,'h')
  
  cells_48h=record_cell[(2+100*movieperh):(101+100*movieperh),]
  matrix_48h=record_matrix[(2+100*movieperh):(101+100*movieperh),]
  agar_48h=record_agar[(2+100*movieperh):(101+100*movieperh),]
  death_48h=record_death[(2+100*movieperh):(101+100*movieperh),]
  
  lattice_matrix=matrix_48h
  lattice_cell=cells_48h
  lattice_agar=agar_48h
  lattice_deathcell=death_48h
  size_matrix=100
  
  
  
  
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
  
  par(new=T)

  
  
  
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
  
  
  max_agar_current=3
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











#compare



dir='path'
setwd(dir) 
record_cell=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
record_matrix=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
record_death=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
record_agar=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")


TL_successunmber_cell=read.table ('cell.csv',header=F,check.names=FALSE,sep=",")
TL_successunmber_matrix=read.table ('matrix.csv',header=F,check.names=FALSE,sep=",")
Total_death_cell=read.table ('death.csv',header=F,check.names=FALSE,sep=",")

cutting_cell=read.table ('cutting_cell.csv',header=F,check.names=FALSE,sep=",")
cutting_death=read.table ('cutting_death.csv',header=F,check.names=FALSE,sep=",")
sum(cutting_cell[(1):(50),51:100])-sum(cutting_death[(1):(50),51:100])
i=24
sum(record_cell[(i*100-98):(i*100-49),51:100])-sum(record_death[(i*100-98):(i*100-49),51:100])

sum(cutting_cell[(51):(100),1:50])-sum(cutting_death[(51):(100),1:50])
sum(cutting_cell)-sum(cutting_cell[(1):(50),51:100])

pdf(file='compare.pdf',width=6,height=4)

startfrom=25
total_h=79

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-98):(i*100-49),51:100]))
}

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-98):(i*100-49),51:100]))
}

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-98):(i*100-49),51:100]))
}

ylim_dynamic=2000

par(mar=c(2,4,1,1))
plot(seq(1,56,1),c(sum(cutting_cell[(1):(50),51:100])-sum(cutting_death[(1):(50),51:100]),TL_successunmber_cell-Total_death_cell),type="l",lty=1,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='red',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")
par(new=TRUE)



startfrom=1
total_h=72

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-98):(i*100+1),1:100])-sum(record_cell[(i*100-98):(i*100-49),51:100]))
}
TL_successunmber_cell=TL_successunmber_cell/3

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-98):(i*100+1),1:100])-sum(record_matrix[(i*100-98):(i*100-49),51:100]))
}
TL_successunmber_matrix=TL_successunmber_matrix/3

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-98):(i*100+1),1:100])-sum(record_death[(i*100-98):(i*100-49),51:100]))
}
Total_death_cell=Total_death_cell/3

ylim_dynamic=2000
plot(seq(1,73,1),c(5,TL_successunmber_cell-Total_death_cell),type="l",lty=2,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='blue',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)


while (!is.null(dev.list()))  dev.off()

pdf(file='2.pdf',width=6,height=4)

startfrom=25
total_h=79

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-98):(i*100-49),51:100]))
}

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-98):(i*100-49),51:100]))
}

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-98):(i*100-49),51:100]))
}

ylim_dynamic=2000

par(mar=c(2,4,1,1))
plot(seq(1,56,1),c(sum(cutting_cell[(1):(50),51:100])-sum(cutting_death[(1):(50),51:100]),TL_successunmber_cell-Total_death_cell),type="l",lty=1,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='red',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")
par(new=TRUE)



startfrom=1
total_h=72

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-48):(i*100+1),51:100]))
}
TL_successunmber_cell=TL_successunmber_cell

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-48):(i*100+1),51:100]))
}
TL_successunmber_matrix=TL_successunmber_matrix

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-48):(i*100+1),51:100]))
}
Total_death_cell=Total_death_cell

ylim_dynamic=2000
plot(seq(1,73,1),c(5,TL_successunmber_cell-Total_death_cell),type="l",lty=2,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='blue',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)


while (!is.null(dev.list()))  dev.off()


[(i*100-48):(i*100+1),51:100]
[(i*100-98):(i*100-49),1:50]
[(i*100-48):(i*100+1),1:50]


#dyna


pdf(file='compare.pdf',width=6,height=4)

startfrom=1
total_h=72

TL_successunmber_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_cell=c(TL_successunmber_cell,sum(record_cell[(i*100-98):(i*100+1),1:100]))
}

TL_successunmber_matrix=vector()
for(i in seq(startfrom,total_h,1))
{
  TL_successunmber_matrix=c(TL_successunmber_matrix,sum(record_matrix[(i*100-98):(i*100+1),1:100]))
}

Total_death_cell=vector()
for(i in seq(startfrom,total_h,1))
{
  Total_death_cell=c(Total_death_cell,sum(record_death[(i*100-98):(i*100+1),1:100]))
}

ylim_dynamic=20000

par(mar=c(2,4,1,1))
plot(seq(1,72,1),TL_successunmber_cell-Total_death_cell,type="l",lty=1,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='red',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)
plot(seq(1,72,1),TL_successunmber_matrix,type="l",lty=1,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='blue',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)
plot(seq(1,72,1),Total_death_cell,type="l",lty=1,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,72),ylim=c(0,ylim_dynamic),bty="L")


while (!is.null(dev.list()))  dev.off()



plot(seq(1,79,1),c(TL_successunmber_cell+TL_successunmber_matrix-Total_death_cell),type="l",lty=2,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='blue',pch=19, ylab="", main="",xlim=c(0,79),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)



plot(seq(1,79,1),c(TL_successunmber_matrix),type="l",lty=2,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='blue',pch=19, ylab="", main="",xlim=c(0,79),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)
plot(seq(1,79,1),c(Total_death_cell),type="l",lty=2,lwd=2,tck=0.01,cex=0.3,las=1,xlab="",col='blue',pch=19, ylab="", main="",xlim=c(0,79),ylim=c(0,ylim_dynamic),bty="L")
par(new=T)
legend(30,1500,legend=c('healing'),cex=1.5,x.intersp=0.5,y.intersp=0,col='red',bty="n",pch='-',lwd=,text.col='black')
par(new=T)
legend(30,1200,legend=c('normal'),cex=1.5,x.intersp=0.5,y.intersp=0,col='blue',bty="n",pch='-',lwd=,text.col='black')
par(new=T)

while (!is.null(dev.list()))  dev.off()






#area

mass=record_cell+record_matrix

movieperh=71

mass_24=record_cell[(2+100*movieperh):(101+100*movieperh),]

matrix_gua=matrix(0,98,98)

for(i in 2:99)
{
  for(j in 2:99)
  {
    matrix_gua[i-1,j-1]=mass_24[i-1,j-1]+mass_24[i,j-1]+mass_24[i+1,j-1]+mass_24[i-1,j]+mass_24[i,j]+mass_24[i+1,j]+mass_24[i-1,j+1]+mass_24[i,j+1]+mass_24[i+1,j+1]
  }
}

hist(matrix_gua[matrix_gua>0],breaks=30)

mean(matrix_gua)

r_test=vector()
x_test=vector()
y_test=vector()

for(i in 2:99)
{
  for(j in 2:99)
  {
   if(matrix_gua[i-1,j-1]>0 & matrix_gua[i-1,j-1]<2)
   {
     r_test=c(r_test,((abs(i-50))^2+(abs(j-50))^2)^0.5) 
     x_test=c(x_test,j)
     y_test=c(y_test,100-i)
   }
  }
}

mean(r_test)/size_matrix*2*1.5


plot(x_test,y_test,xlim=c(0,100),ylim=c(0,100))




#cuttingfig



dir='path/S=1'
setwd(dir)

lattice_cell=read.table ('cutting_cell.csv',header=F,check.names=FALSE,sep=",")
lattice_matrix=read.table ('cutting_matrix.csv',header=F,check.names=FALSE,sep=",")
lattice_deathcell=read.table ('cutting_death.csv',header=F,check.names=FALSE,sep=",")
lattice_agar=read.table ('cutting_agar.csv',header=F,check.names=FALSE,sep=",")

dir='path'
setwd(dir) 
all_file=list.files()
for(read in 2:5)
{
  
  dirsave=paste0('path/',all_file[read])
  setwd(dirsave) 
  lattice_cell=lattice_cell+read.table ('cutting_cell.csv',header=F,check.names=FALSE,sep=",")
  lattice_matrix=lattice_matrix+read.table ('cutting_matrix.csv',header=F,check.names=FALSE,sep=",")
  lattice_deathcell=lattice_deathcell+read.table ('cutting_death.csv',header=F,check.names=FALSE,sep=",")
  lattice_agar=lattice_agar+read.table ('cutting_agar.csv',header=F,check.names=FALSE,sep=",")
  
}

lattice_cell=lattice_cell/5
lattice_matrix=lattice_matrix/5
lattice_deathcell=lattice_deathcell/5
lattice_agar=lattice_agar/5


dir='path'
setwd(dir)




NAME=paste0('t=',24,'h','.pdf')

Nameofpic=paste0('t=',24,'h')

size_matrix=100




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

par(new=T)




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


max_agar_current=3
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


#cut separate

pdf(file=paste0('t=','cut','grey.pdf'),width=9,height=9)

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


pdf(file=paste0('t=','cut','biofilm.pdf'),width=9,height=9)

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





pdf(file=paste0('t=','cut','agar.pdf'),width=9,height=9)


par(mar=c(0, 0, 0, 0))
#mtext(Nameofpic,side=1,line=0,cex=1)


plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")


max_agar_current=3

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





pdf(file=paste0('t=','cut','inactive.pdf'),width=9,height=9)


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


#diameters
dir='path/S=1'
setwd(dir)

record_cell=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
record_matrix=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
record_death=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
record_agar=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")


diameters=vector()

for(movieperh in 1:72)
{
  
  
  NAME=paste0('t=',(movieperh+1)*1,'h','.pdf')
  
  Nameofpic=paste0('t=',(movieperh+1)*1,'h')
  
  cells_48h=record_cell[(2+100*movieperh):(101+100*movieperh),]
  matrix_48h=record_matrix[(2+100*movieperh):(101+100*movieperh),]
  agar_48h=record_agar[(2+100*movieperh):(101+100*movieperh),]
  death_48h=record_death[(2+100*movieperh):(101+100*movieperh),]
  
  lattice_matrix=matrix_48h
  lattice_cell=cells_48h
  lattice_agar=agar_48h
  lattice_deathcell=death_48h
  size_matrix=100
  
  
  
  
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
  
  
  
  
  diameter_x=0
  diameter_y=0
  diameter_i=0
  diameter_j=0
  
  biomass=lattice_cell+lattice_matrix
  
  for(dia in 1:50)
  {
    if(biomass[dia,50]>1)
    {
      diameter_x=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[dia,50]>1)
    {
      diameter_y=dia
      break
    }
  }
  for(dia in 1:50)
  {
    if(biomass[50,dia]>1)
    {
      diameter_i=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[50,dia]>1)
    {
      diameter_j=dia
      break
    }
  }
  
  diameter=(diameter_j-diameter_i)+(diameter_y-diameter_x)
  
  
  for(dia in 1:50)
  {
    if(biomass[dia,51]>1)
    {
      diameter_x=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[dia,51]>1)
    {
      diameter_y=dia
      break
    }
  }
  for(dia in 1:50)
  {
    if(biomass[51,dia]>1)
    {
      diameter_i=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[51,dia]>1)
    {
      diameter_j=dia
      break
    }
  }
  
  diameter=diameter+(diameter_j-diameter_i)+(diameter_y-diameter_x)
  
  
  
  diameter=diameter/4
  diameter=diameter*0.015
  diameters=c(diameters,diameter)
  
  par(new=T)
  f=seq(0,2*pi,0.001)
  x_cir=sin(f)*diameter/1.5*50+50
  y_cir=cos(f)*diameter/1.5*50+50
  plot(x_cir,y_cir,type="l",tck=0.03,cex=0.5,las=1,xlab="",col='black',pch=19, ylab="", main="",xlim=c(0,size_matrix),ylim=c(0,size_matrix),xaxt="n",yaxt="n",bty="o")
  par(new=T)
  
  
  
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
  
  
  max_agar_current=3
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

std_of_diameter=c(0.0084,0.03,0.055,0.195)

pdf(file='Diameter.pdf',width=5,height=4)

plot(seq(1,72,1),diameters,xlim=c(0,100),ylim=c(0,1.5),cex=0.5,pch=3,xlab='T(h)',ylab='Diameter (cm)')
par(new=T)
plot(c(0,24,48,72),c(0.316,0.61,1.17,1.27),xlim=c(0,100),ylim=c(0,1.5),pch=18,cex=0.9,col='red',xlab='',ylab='')
arrows(c(0,24,48,72),c(0.316,0.61,1.17,1.27)-std_of_diameter,c(0,24,48,72),c(0.316,0.61,1.17,1.27)+std_of_diameter,length=0.03,angle=90,col='red',lwd=1.5)
arrows(c(0,24,48,72),c(0.316,0.61,1.17,1.27)+std_of_diameter,c(0,24,48,72),c(0.316,0.61,1.17,1.27)-std_of_diameter,length=0.03,angle=90,col='red',lwd=1.5)

while (!is.null(dev.list()))  dev.off()

#dynamic of diameter

#diameter with errorbar

dir='path'
setwd(dir) 
all_file=list.files()
for(read in 2:5)
{
  diameter_add=vector()
  
  dirsave=paste0('path/',all_file[read])
  setwd(dirsave) 
  for(movieperh in 1:72)
  {
    record_cell=read.table ('record_cell.csv',header=F,check.names=FALSE,sep=",")
    record_matrix=read.table ('record_matrix.csv',header=F,check.names=FALSE,sep=",")
    record_death=read.table ('record_death.csv',header=F,check.names=FALSE,sep=",")
    record_agar=read.table ('record_agar.csv',header=F,check.names=FALSE,sep=",")
    
    
  cells_48h=record_cell[(2+100*movieperh):(101+100*movieperh),]
  matrix_48h=record_matrix[(2+100*movieperh):(101+100*movieperh),]
  agar_48h=record_agar[(2+100*movieperh):(101+100*movieperh),]
  death_48h=record_death[(2+100*movieperh):(101+100*movieperh),]
  
  lattice_matrix=matrix_48h
  lattice_cell=cells_48h
  lattice_agar=agar_48h
  lattice_deathcell=death_48h
  size_matrix=100
  
  
  diameter_x=0
  diameter_y=0
  diameter_i=0
  diameter_j=0
  
  biomass=lattice_cell+lattice_matrix
  
  for(dia in 1:50)
  {
    if(biomass[dia,50]>1)
    {
      diameter_x=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[dia,50]>1)
    {
      diameter_y=dia
      break
    }
  }
  for(dia in 1:50)
  {
    if(biomass[50,dia]>1)
    {
      diameter_i=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[50,dia]>1)
    {
      diameter_j=dia
      break
    }
  }
  
  diameter=(diameter_j-diameter_i)+(diameter_y-diameter_x)
  
  
  for(dia in 1:50)
  {
    if(biomass[dia,51]>1)
    {
      diameter_x=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[dia,51]>1)
    {
      diameter_y=dia
      break
    }
  }
  for(dia in 1:50)
  {
    if(biomass[51,dia]>1)
    {
      diameter_i=dia
      break
    }
  }
  for(dia in 100:51)
  {
    if(biomass[51,dia]>1)
    {
      diameter_j=dia
      break
    }
  }
  
  diameter=diameter+(diameter_j-diameter_i)+(diameter_y-diameter_x)
  
  
  
  diameter=diameter/4
  diameter=diameter*0.015
  diameter_add=c(diameter_add,diameter)
  }
  diameters=rbind(diameters,diameter_add)
}

dir='path'
setwd(dir) 

diameters_sd=vector()
for(diasd in 1:72)
{diameters_sd=c(diameters_sd,sd(diameters[,diasd]))}

insertsd=vector()
for(diasd in 2:71)
{
  insertsd=c(insertsd,(diameters_sd[diasd]+diameters_sd[diasd]+diameters_sd[diasd])/3)
}
insertsd=c(diameters_sd[1],insertsd,diameters_sd[72])

pdf(file='Diameters.pdf',width=6,height=4)
par(mar=c(2, 2, 0, 0))

plot(seq(1,72,1),colSums(diameters)/5,xlim=c(0,75),ylim=c(0,1.5),cex=0.5,pch=3,xlab='',ylab='',col='white',xaxt='n',yaxt='n')
par(new=TRUE)


polygon(c(seq(startfrom,total_h,1),seq(total_h,startfrom,-1)),c(colSums(diameters)/5-insertsd,colSums(diameters[,72:1])/5+insertsd[72:1]), density = NULL, border = F, col = 'lightblue')

par(new=TRUE)

plot(seq(1,72,1),colSums(diameters)/5,xlim=c(0,75),ylim=c(0,1.5),cex=0.3,pch=3,xlab='',ylab='',col='black',xaxt='n',yaxt='n')

par(new=T)
arrows(c(0,24,48,72),c(0.316,0.61,1.17,1.27)-std_of_diameter,c(0,24,48,72),c(0.316,0.61,1.17,1.27)+std_of_diameter,length=0.03,angle=90,col=IndianRed,lwd=3)
par(new=T)
arrows(c(0,24,48,72),c(0.316,0.61,1.17,1.27)+std_of_diameter,c(0,24,48,72),c(0.316,0.61,1.17,1.27)-std_of_diameter,length=0.03,angle=90,col=IndianRed,lwd=3)
par(new=T)
plot(c(0,24,48,72),c(0.316,0.61,1.17,1.27),xlim=c(0,75),ylim=c(0,1.5),pch=18,cex=1,col='red',xlab='',ylab='',xaxt='n',yaxt='n')
axis(side=1,las=1,at=seq(0,75,15),mgp=c(0,0.5,0),tck=0.02,labels=seq(0,75,15),cex.axis=1)
axis(side=2,las=1,at=seq(0,1.5,0.5),mgp=c(0,0.5,0),tck=0.02,las=1,labels=seq(0,1.5,0.5),cex.axis=1)

while (!is.null(dev.list()))  dev.off()
