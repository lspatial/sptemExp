#include "bk_lib.h"

hashVMap getSelfVarMap(vector<pair<double,double>> corSam,vector<double> para,string model){
  hashVMap vsam;
  for(auto i=0;i<corSam.size();i++){
    for(auto j=i;j<corSam.size();j++ ){
      double dist=sqrt(pow(corSam[i].first-corSam[j].first,2)+
                       pow(corSam[i].second-corSam[j].second,2));
      vsam[key(i,j)]=covariogramModel(dist,model,para);
    }
  }
  return vsam;
}

double getSelfVarM(vector<pair<double,double>> corSam,vector<double> para,string model){
  double ret=0,tmpV=0;
  for(auto i=0;i<corSam.size();i++){
    for(auto j=i;j<corSam.size();j++ ){
      double dist=sqrt(pow(corSam[i].first-corSam[j].first,2)+
                       pow(corSam[i].second-corSam[j].second,2));
      double tmp=covariogramModel(dist,model,para);
      ret+=tmp;
      if(i!=j)
        tmpV+=tmp;
    }
  }
  ret=(ret+tmpV)/(double)(corSam.size()*corSam.size());
  return ret;
}

double getTPntCovM(vector<pair<double,double>> tarPnts,pair<double,double> asampnt,
                   vector<double> para, string model){
  double covVM=0;
  for(auto i=0;i<tarPnts.size();i++){
    double dist=sqrt(pow(tarPnts[i].first-asampnt.first,2)+pow(tarPnts[i].second-asampnt.second,2));
    covVM+=covariogramModel(dist,model,para);
  }
  covVM=covVM/(double)tarPnts.size();
  return covVM;
}

double covariogramModel(double dist,string model,vector<double> para){
  //para={range,sill,nugget}
  double ret=-1;
  if(model!="Exp")
    return ret;
  if(dist==0)
    return para[1];
  ret=(para[1]-para[2])*exp(-3*dist/para[0]);
  return ret;
}

pair<double,double> getASolForBlock(vector<pair<double,double>> samPs,vector<double> samz,
                                    vector<pair<double,double>> tarPs, vector<double> para,string model){
  auto n=samz.size();
  hashVMap samVars=getSelfVarMap(samPs,para,model);
  Eigen::MatrixXd mt(n+1,n+1);
  Eigen::VectorXd b(n+1);
  for(int i=0;i<n;i++){
    for(int j=0;j<n;j++){
      mt(i,j)=samVars[key(i,j)];
    }
    mt(i,n)=1;
    b(i)=getTPntCovM(tarPs,samPs[i],para,model);
  }
  for(int j=0;j<n;j++)
    mt(n,j)=1;
  mt(n,n)=0;
  b(n)=1;
  Eigen::VectorXd sol=mt.jacobiSvd(ComputeThinU | ComputeThinV).solve(b);
  Eigen::VectorXd zz= Eigen::Map<Eigen::VectorXd, Eigen::Unaligned>(samz.data(), samz.size());
  Eigen::VectorXd sol1=sol.block(0,0,n,1);
  double mean=(sol1.transpose()*zz)[0];
  double var=getSelfVarM(tarPs,para,model)-(sol.transpose()*b)[0];
  pair<double,double> ret(mean,var);
  return ret;
}

pair<double,double> getMeanV(vector<double> dt){
  double sum = accumulate(begin(dt), end(dt), 0.0);
  double m =  sum / dt.size();

  double accum = 0.0;
  for_each(begin(dt), std::end(dt), [&](const double d) {
    accum += (d - m) * (d - m);
  });
  double var = accum / (dt.size()-1);
  return make_pair(m,var);
};

void testcase(){
  vector<pair<double,double>> psam,ptarget;
  vector<double> zsam;
  for (auto i = 0; i <300 ; ++i) {
    double x=R::runif(0,1) * 2000+1;
    double y=R::runif(0,1) * 3000+1;
    double &&z= (R::runif(0,1) * 5000 + 1) / 500.0;
    psam.push_back(make_pair(x,y));
    zsam.push_back(z);
    if((i%10)==0){
      ptarget.push_back(make_pair(x,y));
    }
  }
  vector<double> paras={800,100,70};
  pair<double,double> ret=getASolForBlock(psam,zsam,ptarget,paras);
  pair<double,double> comR=getMeanV(zsam);
}

