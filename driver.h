#if defined __cplusplus
using Scalar = double;
using Primary = double;
#else
typedef double Scalar;
typedef double Primary;
#endif

typedef struct{
  int * iv;
  int * jv;
  Scalar * vv;
  int m;
  int nnz;
  //int storage;
  //int symmetry;
} mat;

typedef struct{
  Scalar alpha;
  MPI_Comm comm;
  MPI_Fint fcomm;
} parameters;

#if defined __cplusplus
extern "C" {
#endif

  void initmat(mat * Amat);
  void driver_mat(mat * inA, mat * outX, parameters * inParams);
  void driver_mat_mpi(mat * inA, mat * outX, parameters * inParams);
  
#if defined __cplusplus
}
#endif
