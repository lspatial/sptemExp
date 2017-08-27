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
Rcpp::DataFrame noweiAvg(std::string path,std::string preStr="preno2", std::string idStr="id",std::string dateStr="s_date") {
  std::vector<std::string> flist=listdir(path);
  std::unordered_map <std::string, vector<double>> keystats;

  for(unsigned int i=2;i<flist.size();i++){
    //Rcout<<"m:"<<i<<"..."<<endl ;
    string fpath=path+"/"+flist[i];
    Rcpp::DataFrame adf=getCSV(fpath);
    NumericVector prev=adf[preStr];
    StringVector idVec=adf[idStr];
    StringVector sdateVec=adf[dateStr];
    for(unsigned int j=0;j<prev.size();j++){
      string comkey=Rcpp::as<std::string>(idVec[j])+"=="+Rcpp::as<std::string>(sdateVec[j]);
      if(keystats.find(comkey) == keystats.end()){
        vector<double> st;
        double cnt=1;
        double sm=prev[j];
        double sm2=prev[j]*prev[j];
        st.push_back(cnt);
        st.push_back(sm);
        st.push_back(sm2);
        keystats[comkey]=st;
      }else{
        vector<double> st=keystats[comkey];
        st[0]=st[0]+1;
        st[1]=st[1]+prev[j];
        st[2]=st[2]+prev[j]*prev[j];
        keystats[comkey]=st;
      }
    }
  }
  Rcpp::DataFrame res=getDFFromHashMapNoWei(keystats);
  return res ;
}
