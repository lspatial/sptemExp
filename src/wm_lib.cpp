#include "wm_lib.h"


std::string findStr(std::string  input){
  int first_index = input.find_first_of("0123456789");
  string subinput=input.substr(first_index);
  int last_index = subinput.find_first_not_of("0123456789");
  string result = subinput.substr(0, last_index);
  //Rcout<<"input, result "<<input<<"=>"<<result<<endl;
  return result;
}

std::vector<std::string> listdir(std::string path) {
  DIR*    dir;
  dirent* entry;
  std::vector<std::string> files;
  dir = opendir(path.c_str());
  if (dir == NULL) {
    perror("opendir: Path does not exist or could not be read.");
    return files;
  }
  while ((entry = readdir(dir)))
    files.push_back(entry->d_name);
  closedir(dir);
  return files;
}

Rcpp::DataFrame getCSV(std::string path){
  Environment base("package:utils");
  Rcpp::Function read_csv = base["read.csv"];
  Rcpp::DataFrame dfin=read_csv(Rcpp::_["file"] = path,Rcpp::_["row.names"]=R_NilValue);
  return dfin;
}

void printVector(vector<double> in){
  for( auto & val:in){
    Rcout<<val<<"  ";
  }
  Rcout<<endl;
}

Rcpp::DataFrame getDFFromHashMap(std::unordered_map <std::string, vector<double>> keystats){
  Rcpp::List resList(keystats.size());
  StringVector idStr(keystats.size());
  StringVector sdateStr(keystats.size());
  NumericVector means(keystats.size());
  NumericVector std(keystats.size());
  NumericVector var(keystats.size());
  int index=0;
  for(auto i:keystats){
    string key=i.first;
    vector<double> val=i.second;
    int pos=key.find("==");
    idStr[index]=key.substr(0,pos);
    sdateStr[index]=key.substr(pos+2);
    means[index]=val[2];
    var[index]=val[3]/(val[0]);
    std[index]=sqrt(val[3]/(val[0]));
    index++;
    //Rcout<< key<<" ";
    //printVector(val) ;
  }
  Rcpp::List myList(5);
  Rcpp::CharacterVector namevec;
  myList[0] =idStr; // adding vectors
  namevec.push_back("id");

  myList[1]=sdateStr;
  namevec.push_back("date");

  myList[2]=means;
  namevec.push_back("mean");

  myList[3]=std;
  namevec.push_back("StandardDev");

  myList[4]=var;
  namevec.push_back("Variance");

  myList.attr("names") = namevec;
  Rcpp::DataFrame dfout=DataFrame::create(myList,_["stringsAsFactors"] = false);
  return dfout;
}


Rcpp::DataFrame getDFFromHashMapNoWei(std::unordered_map <std::string, vector<double>> keystats){
  Rcpp::List resList(keystats.size());
  StringVector idStr(keystats.size());
  StringVector sdateStr(keystats.size());
  NumericVector means(keystats.size());
  NumericVector std(keystats.size());
  NumericVector var(keystats.size());
  int index=0;
  for(auto i:keystats){
    string key=i.first;
    vector<double> val=i.second;
    int pos=key.find("==");
    idStr[index]=key.substr(0,pos);
    sdateStr[index]=key.substr(pos+2);
    double mn=val[1]/val[0];
    means[index]=mn;
    double vv=val[2]/val[0]-mn*mn ;
    var[index]=vv;
    std[index]=sqrt(vv);
    index++;
  }
  Rcpp::List myList(5);
  Rcpp::CharacterVector namevec;
  myList[0] =idStr; // adding vectors
  namevec.push_back("id");

  myList[1]=sdateStr;
  namevec.push_back("date");

  myList[2]=means;
  namevec.push_back("mean");

  myList[3]=std;
  namevec.push_back("StandardDev");

  myList[4]=var;
  namevec.push_back("Variance");

  myList.attr("names") = namevec;
  Rcpp::DataFrame dfout=DataFrame::create(myList,_["stringsAsFactors"] = false);
  return dfout;
}
