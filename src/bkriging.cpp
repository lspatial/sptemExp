#include <Rcpp.h>
#include <dirent.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <iostream>
#include <fstream>
#include <math.h>

#include "bk_lib.h"

using namespace Rcpp;
using namespace std ;

vector<pair<double,double>> getPairVec(Rcpp::DataFrame df){
  std::vector<double> x=as< std::vector<double> >(df["x"]);
  std::vector<double> y=as< std::vector<double> >(df["y"]);
  // create a vector of length of the smaller vector
  std::vector<std::pair<double, double>> pnts(x.size());
  for (unsigned i = 0; i < pnts.size(); i++)
    pnts[i] = std::make_pair(x[i], y[i]);
  return pnts;
}

// [[Rcpp::export]]
Rcpp::NumericVector bkriging(Rcpp::DataFrame samples, Rcpp::DataFrame rtargets,std::string tarStr,Rcpp::NumericVector paras,std::string model="exponential"){
  vector<double> zval=as< std::vector<double> >(samples[tarStr]);;
  vector<double> paras_=as< std::vector<double> >(paras);
  vector<pair<double,double>> samP=getPairVec(samples);
  //Rcout<<"  SIZE==="<<samP[1].first<<"  "<<samP[samP.size()-1].second<<endl;
  vector<pair<double,double>> tarP=getPairVec(rtargets);
  //Rcout<<tarP[1].first<<" "<<tarP[2].first<<" "<<tarP[3].first<<" "<<tarP[4].first<<" "<<endl;
  pair<double,double>  bksol=getASolForBlock(samP,zval,tarP,paras_,model);
  pair<double,double>  msol=getMeanV(zval);
 // vector<double> ret={bksol.first,bksol.second,msol.first,msol.second};
  vector<double> ret={bksol.first,bksol.second};
  return wrap(ret);
}

//  Sys.setenv("PKG_CXXFLAGS"="-std=c++11")



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

