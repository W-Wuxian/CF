#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <stdlib.h>

#include <mpi.h>
#include "driver.h"

extern "C"
{
  void initmat(mat * Amat){
    Amat->iv = NULL;
    Amat->jv = NULL;
    Amat->vv = NULL;
    Amat->m = 0;
    Amat->nnz = 0;
  }

  void driver_mat(mat * inA, mat * outX, parameters * inParams){
    printf(" nnz %d \n", inA->nnz);
    for(int k =0; k<inA->nnz; ++k){
      printf("%d \n", k);
      outX->iv[k] = inA->iv[k];
      printf("%d \n", k);
      printf(" iv %d \n", inA->iv[k]);
      outX->jv[k] = inA->jv[k];
      outX->vv[k] = inA->vv[k] + inParams->alpha*inA->vv[k];
    }
  }

  void driver_mat_mpi(mat * inA, mat * outX, parameters * inParams){
    int rank;
    //MPI_Fint fcomm;
    //fcomm = inParams->comm;
    //MPI_Comm comm;
    //comm = MPI_Comm_f2c(fcomm);
    if(inParams->fcomm != -1){
      (inParams->comm) = MPI_Comm_f2c((inParams->fcomm));
      printf("fcomm %d \n ", inParams->fcomm);
    }
    MPI_Comm_rank(inParams->comm, &rank);
    
    printf(" nnz %d \n", inA->nnz);
    for(int k =0; k<inA->nnz; ++k){
      printf("%d \n", k);
      outX->iv[k] = inA->iv[k];
      printf("%d \n", k);
      printf(" iv %d \n", inA->iv[k]);
      outX->jv[k] = inA->jv[k];
      if(k==rank){
	outX->vv[k] = rank*inA->vv[k] + rank*inParams->alpha*inA->vv[k];
      }
      else{
	outX->vv[k] = rank;
      }
    }
  }
  
}

/*int main(){
  const int m = 2;
  const int nnz = 2;
  int * a_i = (int *) malloc(nnz * sizeof(int));
  int * a_j = (int *) malloc(nnz * sizeof(int));
  double * a_v = (double *) malloc(nnz * sizeof(double));

  a_i[0]=0;a_j[0]=0;a_v[0]=1;
  a_i[1]=1;a_j[1]=1;a_v[1]=1;

  mat A;
  initmat(&A);
  A.m=m;A.nnz=nnz;
  A.iv=a_i;A.jv=a_j;A.vv=a_v;
  
  int * x_i = (int *) malloc(nnz * sizeof(int));
  int * x_j = (int *) malloc(nnz * sizeof(int));
  double * x_v = (double *) malloc(nnz * sizeof(double));

  x_i[0]=0;x_j[0]=0;x_v[0]=0;
  x_i[1]=0;x_j[1]=0;x_v[1]=0;

  mat X;
  initmat(&X);
  X.m=m;X.nnz=nnz;
  X.iv=x_i;X.jv=x_j;X.vv=x_v;

  parameters params;
  params.alpha = 2.0;
  params.comm = 0;

  driver_mat(&A, &X, &params);

  printf("A:\n");
  for(int k = 0; k < nnz; ++k){ printf("%lf ", a_v[k]); }
  printf("\n");
  printf("X:\n");
  for(int k = 0; k < nnz; ++k){ printf("%lf ", x_v[k]); }
  printf("\n");
  
  free(a_i);
  free(a_j);
  free(a_v);
  free(x_i);
  free(x_j);
  free(x_v);
  return 0;
  }*/

