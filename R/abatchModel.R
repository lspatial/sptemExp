#' @title abatchModel
#'
#' @description A function to batch training of the base models.
#'
#' @param td: Data frame used to train the model
#'
#' @param bnd: A bnd map object to train the spatial effect model, say BayesX
#'
#' @param fS: A vector of formula strings
#'
#' @param iF: Starting index for tids
#'
#' @param iT: Ending index for tids
#'
#' @param tidF : Time id field name
#'
#' @param tids: a vector of time ids
#'
#' @param mPath: The path to save the models trained
#'
#' @param idF: Location id field name
#'
#' @param dateF: The date field
#'
#' @param obsF: Field name of observed values
#'
#' @param nM: Number of the models to be trained
#'
#' @return Trained model saved in the appointed path.
#'
#' @export rmse
#'
abatchModel=function(td,bnd,fS,iF,iT,tidF,tids,mPath,idF="siteid",dateF="date",obsF="obs",nM){
  errorfl=paste(mPath,"/error_",iF,"_",iT,".txt",sep="")
  for(i in c(iF:iT)){ # i=1
    print(paste("t: ",i,sep=" "))
    mydata2f=td[td[,tidF]==tids[i],]
    tidPath=paste(mPath,"/t_",tids[i],"_models",sep="")
    if(!file.exists(tidPath)){
      dir.create(tidPath)
    }
    ids=c(1:nrow(mydata2f))
    for(k in c(1:nM)){ # k=1
      print(paste("       model:",k," of ",nM,sep=""))
      shuffle=sample(ids,replace=TRUE)
      index=unique(shuffle)
      train=mydata2f[index,]
      test=mydata2f[-index,]
      trainedModel=try(R2BayesX::bayesx(as.formula(fS[1]),method = "MCMC",iterations = 20000,data = train),silent = TRUE)
      if(inherits(trainedModel,"try-error")){
        errors=R2BayesX::bayesx_logfile(trainedModel)
        for(a in c(1:length(errors))){ # s=37
          ematch=grep("^ERROR:",errors[a])
          if(length(ematch)>0){
            print("Captured!")
            write.table(k,errorfl,append=TRUE,col.names = FALSE)
            trainedModel=try(R2BayesX::bayesx(as.formula(fS[2]),method = "MCMC",iterations = 20000,data = train),
                             silent = TRUE)
            break
          }
        }
      }
      # bayesx_logfile(trainM)  # save(trainedModel,file="/tmp/md.rda")
      #  saveRDS(trainedModel,file="/tmp/md.rds")
      test$pre=exp(predict(trainedModel,newdata=test))

      adm_r2=rSquared(test[,obsF],test[,obsF]-test$pre)
      adm_rmse=rmse(test[,obsF],test[,obsF]-test$pre)
      adm_Res=data.frame(imodel=k,r2=adm_r2,rmse=adm_rmse)
      modelFile=paste(tidPath,"/m_",k,".rds",sep="")
      saveRDS(trainedModel, modelFile)

      adm_rmse_w=1/adm_rmse
      atest=test[,c(idF,dateF,obsF,"pre")]
      atest$mwei=adm_rmse_w
      if(k==1){
        testADay=atest
      }else{
        testADay=rbind(testADay,atest)
      }

      if(k==1){
        aday_res=adm_Res
      }else{
        aday_res=rbind(aday_res,adm_Res)
      }
      rm(list=c("trainedModel","adm_Res","train","test","shuffle","index","atest","modelFile"))
      gc()
    }
    modelMetricFile=paste(mPath,"/t_",tids[i],"_metrics.csv",sep="")
    write.csv(aday_res,modelMetricFile,row.names =FALSE)

    #cmdStr=paste('plyr::ddply(testADay,c("',idF,'"),plyr::summarize,n=length(pre),pre_sd=SDMTools::wt.sd(pre,mwei),pre=SDMTools::wt.mean(pre,mwei))',sep='')
    cmdStr=paste('plyr::ddply(testADay,c("',idF,'"),plyr::summarize,n=length(pre),pre_sd=sd(pre,na.rm=TRUE),pre=mean(pre,na.rm=TRUE))',sep='')
    testADay_agg=eval(parse(text=cmdStr))
    testADay_agg2=merge(testADay_agg,mydata2f[,c(idF,obsF)],by=idF)

    amidr2=rSquared(testADay_agg2[,obsF],testADay_agg2[,obsF]-testADay_agg2$pre)
    amidrmse=rmse(testADay_agg2[,obsF],testADay_agg2$pre)
    adayRes=data.frame(tid=i,r2=amidr2,rmse=amidrmse)
    resFile=paste(mPath, "/t_",tids[i],'_total_metric.csv',sep="")
    write.csv(adayRes,resFile,row.names =FALSE)
    rm(list=c("mydata2f","testADay_agg","testADay_agg2","adayRes"))
    gc()
  }
}
