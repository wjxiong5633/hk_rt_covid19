#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
#include <RcppParallel.h>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]

using namespace Rcpp ;
using namespace RcppParallel;

//########################################################################################################################################
// function to generate normal random variable
// [[Rcpp::export]]
double rnorm(double a, double b) { // a: mean, b: s.d.
	double c=a+b*sum(rnorm(1));
    return c;
}

//########################################################################################################################################
// function to compute the nb distribution
// [[Rcpp::export]]
double dnb(int n, int m, double c) { 
	
double d=R::dpois(n,(m+1)/c,1);
return d;
}

//########################################################################################################################################
// function to for inverse of logit
// [[Rcpp::export]]
double logit(double a) { 
    return log(a/(1-a));
}

//########################################################################################################################################
// function to for inverse of logit
// [[Rcpp::export]]
double invlogit(double a) { 
    return exp(a)/(1+exp(a));
}

//########################################################################################################################################
// function to generate binomial random number
// [[Rcpp::export]]
int gen_binom(double p){
double cut=(double)rand()/(RAND_MAX);
int out=0;
if (cut<p){
out=1;
}
return out;
}

//########################################################################################################################################
// function to general multiviariable normal given sigma
// [[Rcpp::export]]
NumericVector rmnorm(arma::mat sigma) {
int ncols=sigma.n_cols;
arma::rowvec c=arma::randn(1,ncols);
arma::rowvec a=c*arma::chol(sigma);   
NumericVector b=NumericVector(a.begin(),a.end());   
return b;
}


//########################################################################################################################################
//function to compute the prior likelihood 
// [[Rcpp::export]]
double prior_loglik(NumericVector para){
// check if the para are within their possible range
NumericVector out(para.length());
int b1;
for (b1=9;b1>=0;--b1){
out(b1)=R::dunif(para(b1),-9.99,9.99,1); 
}
for (b1=para.length()-1;b1>=10;--b1){
//out*=R::dnorm(para(b1),5,5,0);
//out*=sum(dunif(NumericVector::create(para(b1)),0.01,19.99));
out(b1)=R::dgamma(para(b1),1.0,5.0,1);
}

double output=sum(out);
if (output < -9999999){
output=-9999999;
}

return output;
}



//########################################################################################################################################
//function to do simulation
// [[Rcpp::export]]
List sim_data(IntegerMatrix data1,
NumericVector para,
NumericVector para2,
NumericMatrix SI,
NumericMatrix inf2report,
int startpt){ 
int b1;
int b2;
int b3;
int b4;

NumericMatrix infpro(data1.nrow(),2);


IntegerMatrix data11(data1.nrow(),data1.ncol());
// initial condition of the data
for (b1=startpt-2;b1>=0;--b1){
data11(b1,1)=5;
data11(b1,3)=5;	
}

for (b1=data11.nrow()-1;b1>=5;--b1){
data11(b1,0)=R::rpois(15);
}

NumericMatrix record(1000,6);

// first set the input date infectiosness
for (b1=startpt-2;b1>=0;--b1){
for (b2=SI.nrow()-1;b2>=0;--b2){
if (b1+b2+1<infpro.nrow()){
infpro(b1+b2+1,0)+=data11(b1,0)*SI(b2,0);
infpro(b1+b2+1,1)+=(data11(b1,1)+data11(b1,3))*SI(b2,1);
}
}
}


// data11 is data by infection time
for (b1=startpt-1;b1<=data11.nrow()-1;++b1){
// first generate infection by current infectiousness
data11(b1,1)=R::rpois(infpro(b1,0)*para(10+b1-(startpt-1)));
data11(b1,3)=R::rpois(infpro(b1,1)*para(10+b1-(startpt-1)+(data11.nrow()-startpt+1)));

// update infectiousness
for (b2=SI.nrow()-1;b2>=0;--b2){
if (b1+b2+1<infpro.nrow()){
infpro(b1+b2+1,0)+=data11(b1,0)*SI(b2,0);
infpro(b1+b2+1,1)+=(data11(b1,1)+data11(b1,3))*SI(b2,1);
}
}
}


IntegerMatrix data12(clone(data11));
IntegerMatrix data13(data11.nrow(),data11.ncol());

// generate unlinked local

// then assign some to unlinked local
int mis=0;
for (b1=data12.nrow()-1;b1>=0;--b1){
if (data12(b1,1)>0){
mis=R::rbinom(data12(b1,1),para2[0]);
data12(b1,2)+=mis;
data12(b1,1)-=mis;
}
if (data12(b1,3)>0){
mis=R::rbinom(data12(b1,3),para2[0]);
data12(b1,2)+=mis;
data12(b1,3)-=mis;
}
}


// generate data by report

for (b1=data11.nrow()-1;b1>=0;--b1){
for (b2=3;b2>=0;--b2){
for (b3=data12(b1,b2)-1;b3>=0;--b3){
double simvalue=R::runif(0,1);
for (b4=inf2report.nrow()-1;b4>=0;--b4){
simvalue-=inf2report(b4,b2);
if (simvalue<0){
if (b1+b4+1<data11.nrow()){	
++data13(b1+b4+1,b2);
}
// break out from the loop 
break;
}
}
}
}
}


return List::create(_[""]=data13,
	_[""]=data12,
	_[""]=data11,
	_[""]=infpro,
	_[""]=record);
} 



//########################################################################################################################################
//function to compute likelihood for infection and symptom
// [[Rcpp::export]]
List loglik(IntegerMatrix data1,
NumericVector para,
NumericMatrix SI,
int smooth,
int startpt){

int b1;
int b2;
int b3;
int b4;

NumericMatrix out1(data1.nrow(),data1.ncol());
NumericMatrix record(data1.nrow(),smooth);
NumericMatrix record2(data1.nrow(),smooth);

// cal the total infectiosuness
NumericMatrix infpro(data1.nrow(),2);
for (b1=data1.nrow()-1;b1>=0;--b1){
for (b2=SI.nrow()-1;b2>=0;--b2){
if (b1+b2+1<infpro.nrow()){

// for local
infpro(b1+b2+1,1)+=data1(b1,2)*SI(b2,1);
}
}
}

double meanvalue=0;
for (b1=data1.nrow()-1;b1>=startpt-1;--b1){
for (b2=smooth-1;b2>=0;--b2){	
if (b1-b2>=0){
// for rt for imported cases
meanvalue=para[10+b1-(startpt-1)]*infpro(b1-b2,1)+0.0000000001;
out1(b1,2)+=R::dpois(data1(b1-b2,2),meanvalue,1);
}
}
}

return List::create(_[""]=out1,
	_[""]=infpro,
	_[""]=SI,
	_[""]=record,
	_[""]=record2);
}






//##############################################################################################################################################
//##############################################################################################################################################
// function for mcmc
// [[Rcpp::export]]
List mcmc(IntegerMatrix data1, 
NumericVector int_para,
NumericVector int_para2, // the missing link probability for the local cases, assume to be equal
NumericMatrix SI, // SI here mean the GI
int smooth, // initial parameter
int startpt,
int mcmc_n,             // length of mcmc stain
NumericVector move,     // which one should move in the model
NumericVector sigma,
NumericVector sigma2){            



// create the vector for use
int b0;
int b1;
int b2;
int b3;
int b4;
int moveindex;

// need to set number of parameter here
NumericMatrix p_para(mcmc_n,int_para.length());
NumericMatrix p_para_r(mcmc_n,sum(move));
p_para(0,_)=int_para;
moveindex=sum(move)-1;
for (b1=int_para.length()-1;b1>=0;--b1){
if (move(b1)){
p_para_r(0,moveindex)=p_para(0,b1);
--moveindex;
}	
}

NumericMatrix p_para2(mcmc_n,int_para2.length());
p_para2(0,_)=int_para2;


// setting augmented data
IntegerMatrix data11(clone(data1));

// matrix to record LL
NumericMatrix LL1(mcmc_n,4);
NumericMatrix LL2(mcmc_n,4);

//####################################################################################################################################
// compute likelihood

List loglikall=loglik(data11,p_para(0,_),SI,smooth,startpt);
List loglikallpro;
NumericMatrix loglik1=loglikall(0);
NumericMatrix loglik1pro;
NumericMatrix data11_loglik_pro;

LL1(0,0)=sum(loglik1);


NumericVector temploglik(1);
NumericVector newloglik(1);
temploglik(0)=LL1(0,0)+prior_loglik(p_para(0,_));


double loglikeratio;
double accept_pro;
NumericVector pro_para(int_para.length());



//####################################################################################################################################
// main mcmc step

// here to record and tune the accept rate for parameter
NumericVector acceptrate(int_para.length());
NumericVector acceptrate2(int_para2.length());
NumericMatrix record(mcmc_n,10);
NumericMatrix imputeacceptrate1(data11.nrow(),data11.ncol());

//####################################################################################################################################
for (b0=1;b0<mcmc_n;++b0){

// after 500 step, then set the sigma to be the empirical sigma
if ((b0>500)&(b0%100==0)){
for (b1=int_para.length()-1;b1>=0;--b1){
if (move(b1)){	
NumericVector temp1(b0-1);
for (b2=b0-2;b2>=0;--b2){
temp1(b2)=p_para(b2,b1);	
}
sigma(b1)=sd(temp1);	
// tuning
if (acceptrate(b1)<=0.1){
sigma(b1)*=0.5;
}	
if ((acceptrate(b1)<=0.15)&(acceptrate(b1)>0.1)){
sigma(b1)*=0.8;
}
if ((acceptrate(b1)<=0.2)&(acceptrate(b1)>0.15)){
sigma(b1)*=0.95;
}
if ((acceptrate(b1)<=0.4)&(acceptrate(b1)>0.3)){
sigma(b1)*=1.05;
}
if ((acceptrate(b1)<=0.9)&(acceptrate(b1)>0.4)){
sigma(b1)*=1.2;
}
if (acceptrate(b1)>0.9){
sigma(b1)*=2;
}
}
}

}


// metorpolis-hasing update on parameter
for (b1=0;b1<int_para.length();++b1){
if (move(b1)){
pro_para=p_para(b0-1,_);
for (b2=b1-1;b2>=0;--b2){
pro_para(b2)=p_para(b0,b2);	
}
pro_para(b1)+=rnorm(0.0,sigma(b1));
newloglik(0)=prior_loglik(pro_para);
if (newloglik(0)> -9999999){
loglikallpro=loglik(data11,pro_para,SI,smooth,startpt);
NumericMatrix tempoutput1=loglikallpro(0);
loglik1pro=clone(tempoutput1);
newloglik(0)+=sum(loglik1pro);
loglikeratio=newloglik(0)-temploglik(0);
accept_pro=pow(exp(1),loglikeratio);
}
else{
accept_pro=0;	
}
if(gen_binom(accept_pro)){
loglik1=clone(loglik1pro);	
p_para(b0,b1)=pro_para(b1);
temploglik(0)=newloglik(0);
acceptrate(b1)*=(b0-1);
acceptrate(b1)+=1;
acceptrate(b1)/=b0;
}
else{
p_para(b0,b1)=p_para(b0-1,b1);
acceptrate(b1)*=(b0-1);
acceptrate(b1)/=b0;
}
}
else {
p_para(b0,b1)=p_para(b0-1,b1);
}
}

LL1(b0,0)=temploglik(0)-prior_loglik(p_para(b0,_));

// update para2 



if (b0%100==1){
Rcout << "b0: " << b0 << std::endl;
}

}


return List::create(_[""]=p_para,
_[""]=p_para2,
_[""]=LL1,
_[""]=LL2,
_[""]=data11,
_[""]=imputeacceptrate1,
_[""]=record,
_[""]=loglik1);
}
