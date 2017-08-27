#include <Rcpp.h>
#include <dirent.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <iostream>
#include <fstream>
#include <math.h>

#include "wm_lib.h"

using namespace Rcpp;
using namespace std ;


// [[Rcpp::export]]
Rcpp::DataFrame weightedstat(std::string path,std::string modelpath,std::string metric="r2",std::string preStr="preno2", std::string idStr="id",std::string dateStr="s_date") {
  //std::string path="/mnt/uscuci/JointNOxStudyUSC/DocumentationNOxModels/output/predict/no2log_str";
  // std::string modelpath="/mnt/uscuci/JointNOxStudyUSC/DocumentationNOxModels/models/no2/mdcv.csv";
  std::vector<std::string> flist=listdir(path);
  std::unordered_map <std::string, double> wei;
  std::unordered_map <std::string, vector<double>> keystats;
  Rcpp::DataFrame weiDF=getCSV(modelpath);
  StringVector keys=weiDF["mid"];
  NumericVector weiV=weiDF[metric];
  //Rcout<<keys.size()<<endl;
  double wsum=0;
  for(int i=0;i<keys.size();i++)
    wsum+=weiV[i];
  for(int i=0;i<keys.size();i++){
    std::string akey=Rcpp::as<std::string>(keys[i]);
    wei[akey]=weiV[i]/wsum;
  }
  string csvpattern=".csv";
  for(unsigned int i=0;i<flist.size();i++){
    //Rcout<<"m:"<<i<<endl ;
    if (flist[i].find(csvpattern)==std::string::npos)
      continue  ;
    string fpath=path+"/"+flist[i];
    Rcpp::DataFrame adf=getCSV(fpath);
    string ikey=findStr(flist[i]);
    //Rcout<<"fpath:"<<fpath<<"; ikey="<<ikey<<endl;
    NumericVector prev=adf[preStr];
    StringVector idVec=adf[idStr];
    StringVector sdateVec=adf[dateStr];
    for(unsigned int j=0;j<prev.size();j++){
      string comkey=Rcpp::as<std::string>(idVec[j])+"=="+Rcpp::as<std::string>(sdateVec[j]);
      // Rcout<<comkey<<"  model:"<<ikey<<endl;
      if(keystats.find(comkey) == keystats.end()){
        vector<double> st;
        double w2=wei[ikey]*wei[ikey];
        double mean=prev[j];
        double S=0;
        st.push_back(wei[ikey]);
        st.push_back(w2);
        st.push_back(mean);
        st.push_back(S);
        keystats[comkey]=st;
      }else{
        vector<double> st=keystats[comkey];
        double oldmean=st[2];
        st[0]=st[0]+wei[ikey];
        st[1]=st[1]+wei[ikey]*wei[ikey];
        st[2]=oldmean+wei[ikey]/st[0]*(prev[j]-oldmean);
        st[3]=st[3]+wei[ikey]*(prev[j]-oldmean)*(prev[j]-st[2]);
        keystats[comkey]=st;
      }
    }
  }
  Rcpp::DataFrame res=getDFFromHashMap(keystats);
  return res ;
}

//  Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
