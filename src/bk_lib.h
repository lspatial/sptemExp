#ifndef SPTEMEXP_BK_LIB_H
#define SPTEMEXP_BK_LIB_H
// [[Rcpp::depends(RcppEigen)]]

#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <functional>
// #include <boost/lambda/lambda.hpp>
#include <algorithm>
// #include <eigen3/Eigen/Dense>
#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace std;
using namespace Rcpp;

//using namespace boost;
//using namespace boost::lambda;
typedef std::istream_iterator<int> in;
using Eigen::MatrixXf;
using namespace Eigen;

struct pair_hash{
  template <class T1, class T2>
  size_t operator()(const pair<T1,T2> &p) const{
    auto h1=hash<T1>{}(p.first);
    auto h2=hash<T2>{}(p.second);
    return h1^h2;
  }
};

inline size_t key(int i,int j) {
  if(i>j)
    return key(j,i);
  return (size_t) i << 32 | (unsigned int) j;
};

using pkey=pair<int,int>;
//using hashDMap=unordered_map<size_t,double,pair_hash>;
using hashVMap=unordered_map<size_t,double>;

hashVMap getSelfVarMap(vector<pair<double,double>> corSam,vector<double> para,string model);
double getSelfVarM(vector<pair<double,double>> corSam,vector<double> para,string model);
double getTPntCovM(vector<pair<double,double>> tarPnts,pair<double,double> asampnt,
                   vector<double> para, string model);
double covariogramModel(double dist,string model,vector<double> para);
pair<double,double> getASolForBlock(vector<pair<double,double>> samPs,vector<double> samz,
                                    vector<pair<double,double>> tarPs, vector<double> para,string model="exponential");
pair<double,double> getMeanV(vector<double> dt);
vector<double> getBKriging();
void testcase();

#endif //SPTEMEXP_BK_LIB_H
