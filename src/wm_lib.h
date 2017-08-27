
#include <Rcpp.h>
#include <dirent.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <iostream>
#include <fstream>
#include <math.h>

using namespace Rcpp;
using namespace std ;


std::string findStr(std::string  input);

std::vector<std::string> listdir(std::string path = ".");

Rcpp::DataFrame getCSV(std::string path);

void printVector(vector<double> in);

Rcpp::DataFrame getDFFromHashMap(std::unordered_map <std::string, vector<double>> keystats) ;

Rcpp::DataFrame getDFFromHashMapNoWei(std::unordered_map <std::string, vector<double>> keystats) ;
