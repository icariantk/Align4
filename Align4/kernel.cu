//Includes for IntelliSense
#define _SIZE_T_DEFINED
#ifndef __CUDACC__
#define __CUDACC__
#endif
#ifndef __cplusplus
#define __cplusplus
#endif

#include <cuda.h>
#include <device_launch_parameters.h>
#include <texture_fetch_functions.h>
#include "float.h"
#include <builtin_types.h>
#include <vector_functions.h>
#include <ctime>
#include  <stdio.h>
extern "C"
{
	__device__ int gap(int val){
		if (val == 0){ return 1; }
		if (val == 1){ return -4; }
		if (val == 2){ return -4; }
		return 0;
	}
	__global__ void align2(int *a, int *b, int *matrix, int *matrixDir, int *scoreMatrix, int m, int n)
	{
		extern __shared__ int order[];
		bool flag = 0;
		int index = blockIdx.x*blockDim.x + threadIdx.x;
		int x = threadIdx.x + 1;
		int topLeft, left, top;

		if (index == 0){
			for (int c = 0; c != m; c++){
				order[c] = 0;
			}
		}
		if (index < m){
			for (int c = 0; c != n; c++){
				matrix[c*m + index] = 0;
			}
		}

		__syncthreads();
		if (index < m - 1){
			for (int c = 0; c != n; c++)
			{
				if (index == 0)
				{
					matrixDir[0] = 0;
					matrixDir[c*m + index] = 3;
				}
				else
				{
					if (c == 0)
					{
						matrixDir[c*m + index] = 2;
					}
					else
					{
						matrixDir[c*m + index] = -1;
					}
				}
			}

			__syncthreads();

			for (int y = 1; y != n; y++){
				if (index == 0){
					order[0] = y;
					flag = 1;
				}
				else{
					if (order[index - 1] >= y){
						order[index] = y;
						flag = 1;
					}
					else{
						flag = 0;
					}
				}
				__syncthreads();

				if (flag){
					if ((a[index] != '-') && (b[y - 1] != '-')){
						topLeft = matrix[((y - 1)*m) + (x - 1)] + scoreMatrix[((a[x - 1] - 65) * 27) + (b[y - 1] - 65)];
					}
					else{
						topLeft = gap(1);
					}
					top = matrix[((y - 1)*m) + x] + gap(2);
					left = matrix[(y*m) + (x - 1)] + gap(2);

					if (topLeft >= left&&topLeft >= top){ matrixDir[y*m + x] = 1; matrix[y*m + x] = topLeft; }
					if (top > topLeft&&top >= left){ matrixDir[y*m + x] = 3; matrix[y*m + x] = top; }
					if (left > topLeft&&left > top){ matrixDir[y*m + x] = 2; matrix[y*m + x] = left; }
				}
				else{
					y--;
				}
				if (y == n - 1){
					order[index]++;
				}

				__syncthreads();
			}
		}
	}

	__global__ void traceback(int a[], int ai[], int b[], int bi[], int matrixDir[], int m, int n, int *k)
	{
		int x = m - 1;
		int y = n - 1;
		int c = 0;

		while (!(x == 0 && y == 0)){
			if (matrixDir[y*m + x] == 3){ ai[c] = '-'; if (y > 0){ bi[c] = b[y - 1]; y--; } }
			else
				if (matrixDir[y*m + x] == 2){ bi[c] = '-'; if (x > 0){ ai[c] = a[x - 1]; x--; } }
				else
					if (matrixDir[y*m + x] == 1){ if (x > 0 && y > 0){ ai[c] = a[x - 1]; bi[c] = b[y - 1]; x--; y--; } }
					else{
						x = 0; y = 0;
					}
					c++;
		}

		ai[c] = '\0';
		bi[c] = '\0';

		k[0] = c;
		k[0]--;
	}

	__global__ void invert(int a[], int ai[], int b[], int bi[], int k[])
	{
		int index = blockIdx.x*blockDim.x + threadIdx.x;
		if (index < k[0]){
			int vala = ai[k[0] - index - 1];
			int valb = bi[k[0] - index - 1];
			__syncthreads();
			a[index] = vala;
			b[index] = valb;
			__syncthreads();
		}
	}

	__global__ void alignPSP(int *a, int *b, int *matrix, int *matrixDir, int *scoreMatrix, int am, int an, int bm, int bn, int gap0, int gap1, int gap2, int offset, int size, int *order)
	{
		//gap0=gap gap, gap1=gap opening, gap2=gap mistmatch
		
		bool flag = 0;
		int index = (blockIdx.x*blockDim.x + threadIdx.x) + (offset * size);
		int x = index + 1;
		int topLeft, left, top;
		//printf("Soy el index: %d\n", index);
		//if (index == 0){
		//	printf("gaps inside cuda: %d %d %d\n", gap0, gap1, gap2);
		//}
		if (index < an+1){
			order[index] = 0;
			
				//matrix[c*(an + 1) + x] = 0;

				if (index == 0){
					for (int c = 0; c != bn + 1; c++){
						matrix[c*(an + 1)] = gap2*c;
					}
				} else{
					matrix[x] = gap2*x;
					matrixDir[x] = 2;
				}
			}

		

		__syncthreads();
		__threadfence_block();
		if (index < an+1){
			if (index == 0){
				for (int c = 0; c != bn + 1; c++)
				{
					matrixDir[c*(an + 1)] = 3;
				}
			}
			else{
				matrixDir[index] = 2;
			}
		}
		if (index == 0){
		matrixDir[0] = 0;
		}

		__syncthreads();
		__threadfence_block();
		if (index < an){
			//printf("\n%d 1\n", index);
			for (int y = 1; y <= bn; y++){
				if (index == 0){
					order[0] = y;
					flag = 1;
				}
				else{
					if (order[index - 1] > y){
						flag = 1;
					}
					else{
						flag = 0;
						y--;
					}
				}
				__syncthreads();
				__threadfence_block();

				if (flag){
					//	printf("\n%d 2\n", index);
					int sum = 0;
					for (int xx = 0; xx != am; xx++){
						for (int yy = 0; yy != bm; yy++){
							sum += (a[xx*an + (x - 1)] != '-'&&b[yy*bn + (y - 1)] != '-') ? scoreMatrix[((a[xx*an + (x - 1)] - 65) * 27) + (b[yy*bn + (y - 1)] - 65)] : a[xx*an + (x - 1)] == b[yy*bn + (y - 1)] ? gap0 : gap2;
							//	printf("Se comparó: %c y %c y salio: %d\n", a[xx*an + (x - 1)], b[yy*bn + (y - 1)], scoreMatrix[((a[xx*an + (x - 1)] - 65) * 27) + (b[yy*bn + (y - 1)] - 65)]);
						}
					}
					//Agregar el Gap OPENING

					topLeft = matrix[(y - 1)*(an + 1) + (x - 1)] + sum;

					sum = 0;
					for (int xx = 0; xx != am; xx++){
						for (int yy = 0; yy != bm; yy++){
							sum += a[xx*an + (x - 1)] == '-' ? gap0 : gap2;
						}
					}
					/*if (y > 1)
						sum += matrixDir[((y - 1)*(an + 1)) + x] == 1 ? gap1*am : 0;*/

					top = matrix[((y - 1)*(an + 1)) + x] + sum;

					sum = 0;
					for (int xx = 0; xx != am; xx++){
						for (int yy = 0; yy != bm; yy++){
							sum += b[yy*bn + (y - 1)] == '-' ? gap0 : gap2;
						}
					}
					/*if (x > 1)
						sum += matrixDir[y*(an + 1) + x - 1] == 1 ? gap1*bm : 0;*/

					left = matrix[(y*(an + 1)) + (x - 1)] + sum;

					//printf("matrix[%d,%d] top:%d topleft:%d left:%d\n", x, y, top, topLeft, left);
					if (topLeft >= left&&topLeft >= top){
						matrixDir[y*(an + 1) + x] = 1;
						matrix[y*(an + 1) + x] = topLeft;
						//	printf("matrix[%d,%d]=%d,%d\n", x, y, matrix[y*(an + 1) + x], matrixDir[y*(an + 1) + x]);
					}

					if (top > topLeft&&top >= left){
						matrixDir[y*(an + 1) + x] = 3;
						matrix[y*(an + 1) + x] = top;
						//printf("matrix[%d,%d]=%d,%d\n", x, y, matrix[y*(an + 1) + x], matrixDir[y*(an + 1) + x]);
					}

					if (left > topLeft&&left > top)
					{
						matrixDir[y*(an + 1) + x] = 2;
						matrix[y*(an + 1) + x] = left;
						//	printf("matrix[%d,%d]=%d,%d\n", x, y, matrix[y*(an + 1) + x], matrixDir[y*(an + 1) + x]);
					}
					//	printf("\n%d 3\n", index);
					/*if (index == 84&&y==90){
					printf("\nIndex 84 matrixDir[%d,%d]=%d\n", x, y, matrixDir[y*(an + 1) + x]);
					printf("matrix[%d,%d] top:%d topleft:%d left:%d\n", x, y, top, topLeft, left);
					}*/
					if (y == bn){
						order[index]++;
					}
					order[index]++;
				}
				__syncthreads();
			}
			//printf("\n%d 4\n", index);
			order[index]++;
			order[index]++;
			__syncthreads();
			__threadfence_block();
		}
	}

	__global__ void align2SIMO_Initialize(int *sizes, int x, int y, int sqrZone){
	}
	__global__ void kMerDistance(int *matrix, int *matrixDir, int *indexes, int *sequences, int *sizes, int nseq, int *scoreMatrix, int x, int y, int gap0, int gap1, int gap2, int *score, int sqrZone, int K)
	{
		//gap0=gap gap, gap1=gap opening, gap2=gap mistmatch

		int index = blockIdx.x*blockDim.x + threadIdx.x;
		int seqA = x*sqrZone + (index % sqrZone);
		bool flag;
		int seqB = y*sqrZone + (index / sqrZone);
		int sum = 0;
		int c, d, k;
		int *kmer;
		int sumA, sumB;
		if (x <= y){
			if (seqB < nseq&&seqA < nseq){
				int *A, *B;
				int offsetA = 0;
				int offsetB = 0;
				if (seqB > seqA){
					for (c = 0; c <= seqB; c++){
						if (c == seqA){ offsetA = sum; }
						if (c == seqB){ offsetB = sum; }
						sum += sizes[c];
					}
					A = &sequences[offsetA];
					B = &sequences[offsetB];
					for (d = 0; d != sizes[seqA] - K; d++){
						kmer = &A[d];

						for (c = 0; c != sizes[seqA] - K; c++){
							flag = true;
							for (k = 0; k != K; k++){
								if (kmer[k] != A[c + k]){
									flag = false;
									break;
								}
							}
							sumA += flag;
						}
						for (c = 0; c != sizes[seqB] - K; c++){
							flag = true;
							for (k = 0; k != K; k++){
								if (kmer[k] != B[c + k]){
									flag = false;
									break;
								}
							}
							sumB += flag;
						}
						if (sumA < sumB){
							sum += sumA;
						}
						else{
							sum += sumB;
						}
					}
					if (sizes[seqA] < sizes[seqB]){
						sum /= sizes[seqA] - K + 1;
					}
					else{
						sum /= sizes[seqB] - K + 1;
					}

					score[seqA*nseq + seqB] = sum;
					score[seqB*nseq + seqA] = sum;
				}
			}
		}
	}

	__global__ void align2SIMO(int *matrix, int *matrixDir, int *indexes, int *sequences, int *sizes, int nseq, int *scoreMatrix, int x, int y, int gap0, int gap1, int gap2, int *score, int sqrZone, int mode)
	{
		//gap0=gap gap, gap1=gap opening, gap2=gap mistmatch
		//mode  0=Column score 1=propossal
		int index = blockIdx.x*blockDim.x + threadIdx.x;
		int seqA = x*sqrZone + (index % sqrZone);
		int seqB = y*sqrZone + (index / sqrZone);
		//printf("\nholly\n");
		if (x <= y){
			/*		if (index == 0){
			matrix = new int**[(sqrZone*sqrZone)];
			matrixDir = new int**[(sqrZone*sqrZone)];
			for (int ccc = 0; ccc != sqrZone*sqrZone; ccc++){
			matrix[ccc] = new int*[sizes[x*sqrZone + (ccc % sqrZone)] + 1];
			matrixDir[ccc] = new int*[sizes[x*sqrZone + (ccc % sqrZone)] + 1];
			}
			for (int ccc = 0; ccc != sqrZone*sqrZone; ccc++){
			for (int d = 0; d != sizes[x*sqrZone + (ccc % sqrZone)] + 1; d++){
			matrix[ccc][d] = new int[sizes[y*sqrZone + (ccc / sqrZone)] + 1];
			matrixDir[ccc][d] = new int[sizes[y*sqrZone + (ccc / sqrZone)] + 1];
			}
			}
			}
			__syncthreads();
			*/
			//printf("\nholly\n");
			/**/
			if (seqB < nseq&&seqA < nseq){
				int offsetA = 0;
				int offsetB = 0;
				int sum = 0;
				int m = sizes[seqA];
				int n = sizes[seqB];
				/*int* A = new int[m];
				int* B = new int[n];*/
				int *A, *B;
				int *MatrixDir, *Matrix;
				int X, Y;
				if (seqB > seqA){
					for (int c = 0; c <= seqB; c++){
						if (c == seqA){ offsetA = sum; }
						if (c == seqB){ offsetB = sum; }
						sum += sizes[c];
					}
					//printf("\n2");

					int sSeqA = sizes[seqA] + 1;

					int offmatrix = indexes[index];
					Matrix = &matrix[offmatrix];
					MatrixDir = &matrixDir[offmatrix];

					for (X = 0; X != m + 1; X++){
						MatrixDir[X] = 2;
						Matrix[X] = 0;
					}
					for (Y = 0; Y != n + 1; Y++){
						MatrixDir[Y*sSeqA] = 3;
						Matrix[Y*sSeqA] = 0;
					}
					MatrixDir[0] = 0;
					Matrix[0] = 0;
					/*
					for (int x = 0; x != m + 1; x++){
					for (int y = 0; y != n + 1; y++){
					//printf("[%d](%d,%d) [ ]\n", blockIdx.x*blockDim.x + threadIdx.x, x, y, m, n);

					Matrix[ y*(sizes[seqA] + 1) + x] = 0;
					if (y == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 2;
					}
					else{
					MatrixDir[ y*(sizes[seqA] + 1) + x] = -1;
					}
					if (x == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 3;
					}
					if (x == 0 && y == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 0;
					}
					//printf("[%d](%d,%d) [X]\n", blockIdx.x*blockDim.x + threadIdx.x, x, y, m, n);
					}
					}
					*/

					//printf("\n3");
					A = &sequences[offsetA];
					B = &sequences[offsetB];

					/*
					for (int c = offsetA; c != offsetA + m; c++){
					A[c - offsetA] = sequences[c];
					}
					for (int c = offsetB; c != offsetB + n; c++){
					B[c - offsetB] = sequences[c];
					}*/

					//printf("\nFin de la inicializacion\n");
					for (int x = 1; x != m + 1; x++){
						for (int y = 1; y != n + 1; y++){
							int topLeft = 0, left = 0, top = 0;
							topLeft = Matrix[(y - 1)*(sSeqA)+(x - 1)] + scoreMatrix[((A[x - 1] - 65) * 27) + (B[y - 1] - 65)];
							top = Matrix[(y - 1)*(sSeqA)+(x)] - gap2;
							top += MatrixDir[(y - 1)*(sSeqA)+(x)] == 1 ? gap1 : 0;
							left = Matrix[(y)*(sSeqA)+(x - 1)] - gap2;
							left += MatrixDir[(y)*(sSeqA)+(x - 1)] == 1 ? gap1 : 0;
							if (topLeft >= left&&topLeft >= top){
								MatrixDir[(y)*(sSeqA)+(x)] = 1;
								Matrix[(y)*(sSeqA)+(x)] = topLeft;
							}
							if (top > topLeft&&top >= left){
								MatrixDir[(y)*(sSeqA)+(x)] = 3;
								Matrix[(y)*(sSeqA)+(x)] = top;
							}
							if (left > top&&left > topLeft)
							{
								MatrixDir[(y)*(sSeqA)+(x)] = 2;
								Matrix[(y)*(sSeqA)+(x)] = left;
							}
						}
					}

					//	printf("\nFin de valores a la matriz\n");
					X = m;
					Y = n;
					int scor = 0;

					while (!(X == 0 && Y == 0)){
						switch (MatrixDir[(Y)*(sSeqA)+(X)]){
						case 1: X--; Y--; break;
						case 2: X--; scor++; break;
						case 3: Y--; scor++; break;
						default: break;
						}
					}

					score[seqA*nseq + seqB] = scor;
					score[seqB*nseq + seqA] = scor;
					//	printf("\n[%d,%d]=%d\n",seqA,seqB,scor);
				}
				else{
					if (seqA == seqB)score[seqA*nseq + seqB] = 0;
				}
			}
		}
	}
	__global__ void align2SIMO_g(int *matrix, int *matrixDir, int *indexes, int *sequences, int *sizes, int nseq, int *scoreMatrix, int x, int y, int gap0, int gap1, int gap2, double *score, int sqrZone, int r)
	{
		//gap0=gap gap, gap1=gap opening, gap2=gap mistmatch
		//mode  0=Column score 1=propossal
		int index = blockIdx.x*blockDim.x + threadIdx.x;
		int seqA = x*sqrZone + (index % sqrZone);
		int seqB = y*sqrZone + (index / sqrZone);
		//printf("\nholly\n");
		if (x <= y){
			/*		if (index == 0){
			matrix = new int**[(sqrZone*sqrZone)];
			matrixDir = new int**[(sqrZone*sqrZone)];
			for (int ccc = 0; ccc != sqrZone*sqrZone; ccc++){
			matrix[ccc] = new int*[sizes[x*sqrZone + (ccc % sqrZone)] + 1];
			matrixDir[ccc] = new int*[sizes[x*sqrZone + (ccc % sqrZone)] + 1];
			}
			for (int ccc = 0; ccc != sqrZone*sqrZone; ccc++){
			for (int d = 0; d != sizes[x*sqrZone + (ccc % sqrZone)] + 1; d++){
			matrix[ccc][d] = new int[sizes[y*sqrZone + (ccc / sqrZone)] + 1];
			matrixDir[ccc][d] = new int[sizes[y*sqrZone + (ccc / sqrZone)] + 1];
			}
			}
			}
			__syncthreads();
			*/
			//printf("\nholly\n");
			/**/
			if (seqB < nseq&&seqA < nseq){
				int offsetA = 0;
				int offsetB = 0;
				int sum = 0;
				int m = sizes[seqA];
				int n = sizes[seqB];
				/*int* A = new int[m];
				int* B = new int[n];*/
				int *A, *B;
				int *MatrixDir, *Matrix;
				int X, Y;
				if (seqB > seqA){
					for (int c = 0; c <= seqB; c++){
						if (c == seqA){ offsetA = sum; }
						if (c == seqB){ offsetB = sum; }
						sum += sizes[c];
					}
					//printf("\n2");

					int sSeqA = sizes[seqA] + 1;
					int sSeqB = sizes[seqB];
					int offmatrix = indexes[index];
					Matrix = &matrix[offmatrix];
					MatrixDir = &matrixDir[offmatrix];

					for (X = 0; X != m + 1; X++){
						MatrixDir[X] = 2;
						Matrix[X] = 0;
					}
					for (Y = 0; Y != n + 1; Y++){
						MatrixDir[Y*sSeqA] = 3;
						Matrix[Y*sSeqA] = 0;
					}
					MatrixDir[0] = 0;
					Matrix[0] = 0;
					/*
					for (int x = 0; x != m + 1; x++){
					for (int y = 0; y != n + 1; y++){
					//printf("[%d](%d,%d) [ ]\n", blockIdx.x*blockDim.x + threadIdx.x, x, y, m, n);

					Matrix[ y*(sizes[seqA] + 1) + x] = 0;
					if (y == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 2;
					}
					else{
					MatrixDir[ y*(sizes[seqA] + 1) + x] = -1;
					}
					if (x == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 3;
					}
					if (x == 0 && y == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 0;
					}
					//printf("[%d](%d,%d) [X]\n", blockIdx.x*blockDim.x + threadIdx.x, x, y, m, n);
					}
					}
					*/

					//printf("\n3");
					A = &sequences[offsetA];
					B = &sequences[offsetB];
					for (int x = 1; x != m + 1; x++){
						for (int y = 1; y != n + 1; y++){
							int topLeft = 0, left = 0, top = 0;
							topLeft = Matrix[(y - 1)*(sSeqA)+(x - 1)] + scoreMatrix[((A[x - 1] - 65) * 27) + (B[y - 1] - 65)];
							top = Matrix[(y - 1)*(sSeqA)+(x)] + gap2;
							//top += MatrixDir[(y - 1)*(sSeqA)+(x)] == 1 ? gap1 : 0;
							left = Matrix[(y)*(sSeqA)+(x - 1)] + gap2;
							//left += MatrixDir[(y)*(sSeqA)+(x - 1)] == 1 ? gap1 : 0;
							if (topLeft >= left&&topLeft >= top){
								MatrixDir[(y)*(sSeqA)+(x)] = 1;
								Matrix[(y)*(sSeqA)+(x)] = topLeft;
							}
							if (top > topLeft&&top >= left){
								MatrixDir[(y)*(sSeqA)+(x)] = 3;
								Matrix[(y)*(sSeqA)+(x)] = top;
							}
							if (left > top&&left > topLeft)
							{
								MatrixDir[(y)*(sSeqA)+(x)] = 2;
								Matrix[(y)*(sSeqA)+(x)] = left;
							}
						}
					}

					//	printf("\nFin de valores a la matriz\n");
					X = m;
					Y = n;
					score[0] = 0;
					double  scor = 0;
					int *a_inv = new int[sSeqA + sSeqB];
					int *b_inv = new int[sSeqA + sSeqB];
					int size = 0;

					while (!(X == 0 && Y == 0)){
						switch (MatrixDir[(Y*sSeqA) + X]){
						case 1:	a_inv[size] = A[X - 1]; b_inv[size] = B[Y - 1]; X--; Y--; break;
						case 2: a_inv[size] = A[X - 1]; b_inv[size] = 45; X--; break;
						case 3: a_inv[size] = 45; b_inv[size] = B[Y - 1]; Y--; break;
						default: break;
						}
						size++;
					}

					for (int c = 0; c != size; c++){
						if (a_inv[c] == b_inv[c]){ scor++; }
					}
					score[seqA*nseq + seqB] = scor / (double)size;
					score[seqB*nseq + seqA] = scor / (double)size;

				}
				else{
					if (seqA == seqB)score[seqA*nseq + seqB] = 0;
				}
			}
		}
	}



	__global__ void align2SIMO_r(int *matrix, int *matrixDir, int *indexes, int *sequences, int *sizes, int nseq, int *scoreMatrix, int x, int y, int gap0, int gap1, int gap2, double *score, int sqrZone, int r)
	{
		//gap0=gap gap, gap1=gap opening, gap2=gap mistmatch
		//mode  0=Column score 1=propossal
		int index = blockIdx.x*blockDim.x + threadIdx.x;

		int seqA = x*sqrZone + (index % sqrZone);
		int seqB = y*sqrZone + (index / sqrZone);
		//printf("\nholly\n");
		if (x <= y){
			/*		if (index == 0){
			matrix = new int**[(sqrZone*sqrZone)];
			matrixDir = new int**[(sqrZone*sqrZone)];
			for (int ccc = 0; ccc != sqrZone*sqrZone; ccc++){
			matrix[ccc] = new int*[sizes[x*sqrZone + (ccc % sqrZone)] + 1];
			matrixDir[ccc] = new int*[sizes[x*sqrZone + (ccc % sqrZone)] + 1];
			}
			for (int ccc = 0; ccc != sqrZone*sqrZone; ccc++){
			for (int d = 0; d != sizes[x*sqrZone + (ccc % sqrZone)] + 1; d++){
			matrix[ccc][d] = new int[sizes[y*sqrZone + (ccc / sqrZone)] + 1];
			matrixDir[ccc][d] = new int[sizes[y*sqrZone + (ccc / sqrZone)] + 1];
			}
			}
			}
			__syncthreads();
			*/
			
			/**/
			
			if (seqB < nseq&&seqA < nseq){
				
				int offsetA = 0;
				int offsetB = 0;
				int sum = 0;
				int m = sizes[seqA];
				int n = sizes[seqB];
				/*int* A = new int[m];
				int* B = new int[n];*/
				int *A, *B;
				int *MatrixDir, *Matrix;
				int X, Y;
				if (seqB > seqA){
					for (int c = 0; c <= seqB; c++){
						if (c == seqA){ offsetA = sum; }
						if (c == seqB){ offsetB = sum; }
						sum += sizes[c];
					}
					

					int sSeqA = sizes[seqA] + 1;
					int sSeqB = sizes[seqB];
					int offmatrix = indexes[index];
					Matrix = &matrix[offmatrix];
					MatrixDir = &matrixDir[offmatrix];

					for (X = 0; X != m + 1; X++){
						MatrixDir[X] = 2;
						Matrix[X] = 0;
					}
					for (Y = 0; Y != n + 1; Y++){
						MatrixDir[Y*sSeqA] = 3;
						Matrix[Y*sSeqA] = 0;
					}
					MatrixDir[0] = 0;
					Matrix[0] = 0;
					/*
					for (int x = 0; x != m + 1; x++){
					for (int y = 0; y != n + 1; y++){
					//printf("[%d](%d,%d) [ ]\n", blockIdx.x*blockDim.x + threadIdx.x, x, y, m, n);

					Matrix[ y*(sizes[seqA] + 1) + x] = 0;
					if (y == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 2;
					}
					else{
					MatrixDir[ y*(sizes[seqA] + 1) + x] = -1;
					}
					if (x == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 3;
					}
					if (x == 0 && y == 0){
					MatrixDir[y*(sizes[seqA] + 1) + x] = 0;
					}
					//printf("[%d](%d,%d) [X]\n", blockIdx.x*blockDim.x + threadIdx.x, x, y, m, n);
					}
					}
					*/
					/*
					if (seqA == 0 && seqB == 1){
					printf("Blosum:\n");
					for (int c = 0; c != 10; c++){
						printf("%d ", scoreMatrix[c]);
					}
					}*/
					
					A = &sequences[offsetA];
					B = &sequences[offsetB];

					for (int x = 1; x != m + 1; x++){
						for (int y = 1; y != n + 1; y++){
							int topLeft = 0, left = 0, top = 0;
							topLeft = Matrix[(y - 1)*(sSeqA)+(x - 1)] + scoreMatrix[((A[x - 1] - 65) * 27) + (B[y - 1] - 65)];
							top = Matrix[(y - 1)*(sSeqA)+(x)] + gap2;
							top += MatrixDir[(y - 1)*(sSeqA)+(x)] == 1 ? gap1 : 0;
							left = Matrix[(y)*(sSeqA)+(x - 1)] + gap2;
							left += MatrixDir[(y)*(sSeqA)+(x - 1)] == 1 ? gap1 : 0;
							if (topLeft >= left&&topLeft >= top){
								MatrixDir[(y)*(sSeqA)+(x)] = 1;
								Matrix[(y)*(sSeqA)+(x)] = topLeft;
							}
							if (top > topLeft&&top >= left){
								MatrixDir[(y)*(sSeqA)+(x)] = 3;
								Matrix[(y)*(sSeqA)+(x)] = top;
							}
							if (left > top&&left > topLeft)
							{
								MatrixDir[(y)*(sSeqA)+(x)] = 2;
								Matrix[(y)*(sSeqA)+(x)] = left;
							}
						}
					}

					
					X = m;
					Y = n;
					score[0] = 0;
					double  scor = 0;
					int *a_inv = new int[sSeqA + sSeqB];
					int *b_inv = new int[sSeqA + sSeqB];
					int size = 0;

					while (!(X == 0 && Y == 0)){
						switch (MatrixDir[(Y*sSeqA) + X]){
						case 1:	a_inv[size] = A[X - 1]; b_inv[size] = B[Y - 1]; X--; Y--; break;
						case 2: a_inv[size] = A[X - 1]; b_inv[size] = 45; X--; break;
						case 3: a_inv[size] = 45; b_inv[size] = B[Y - 1]; Y--; break;
						
						}
						size++;
					}
					double g=0, h=0;
					for (int c = 0; c != size; c++){
						for (int d = c - r; d != c + r; d++){
							if (d >= 0 && d < size){
								if (d < c){
									
										if (a_inv[c] != '-'&&b_inv[d] != '-'){
											g = (double)scoreMatrix[((a_inv[c] - 65) * 27) + (b_inv[d] - 65)];
										}
										else{
											g = (double)gap0;
										}
										g += 4 - gap1;
										h = r - (d - (c - r)) + 1.0;

										scor += (h / g);
									
								}
								if (d == c){
									if (a_inv[c] != '-'&&b_inv[d] != '-'){
										scor += (double)scoreMatrix[((a_inv[c] - 65) * 27) + (b_inv[d] - 65)];
									}
									else{
										scor += gap0;
									}
								}
								if (d > c){
									if (d < size){
										if (a_inv[c] != '-'&&b_inv[d] != '-'){
											g = (double)scoreMatrix[((a_inv[c] - 65) * 27) + (b_inv[d] - 65)];
										}
										else{
											g = gap0;
										}
										g += 4 - gap1;
										h = (d - (c - r)) + 1.0;
										scor += (h / g);
									}
								}
							}
						}
					}

					score[seqA*nseq + seqB] = scor;
					score[seqB*nseq + seqA] = scor;
					/*
					printf("\n[%d,%d] size %d score:%f\n", m, n, size, scor);
					if (seqA == 0 && seqB == 1){
					for (int c = 0; c != size; c++){
						printf("%c", a_inv[c]);
					}
					
						printf("\n");
					
					for (int c = 0; c != size; c++){
						printf("%c", b_inv[c]);
					}
					printf("\n");
					}*/
				}
				else{
					if (seqA == seqB)score[seqA*nseq + seqB] = 0;
				}
			}
		}
	}

	__global__ void tracebackPSP(int inv[], int a[], int b[], int matrixDir[], int am, int an, int bm, int bn, int *k)
	{
		int x = an;
		int y = bn;
		int c = 0;

		int maxInv = (an + bn);
		for (int k = 0; k != maxInv*(am + bm); k++){
			inv[k] = 64;
		}

		while (!(x == 0 && y == 0)){
			if (matrixDir[y*(an + 1) + x] == 3){
				for (int d = 0; d != am; d++){
					inv[(d*maxInv) + c] = '-';
				}
				for (int d = am; d != am + bm; d++){
					inv[((d*maxInv) + c)] = b[(d - am)*bn + y - 1];
				}
				y--;
			}
			else
			{
				if (matrixDir[y*(an + 1) + x] == 2)
				{
					for (int d = 0; d != am; d++){
						inv[(d*maxInv) + c] = a[d*an + x - 1];
					}
					for (int d = am; d != am + bm; d++){
						inv[((d*maxInv) + c)] = '-';
					}
					x--;
				}
				else
				{
					if (matrixDir[y*(an + 1) + x] == 1)
					{
						for (int d = 0; d != am; d++){
							inv[(d*maxInv) + c] = a[d*an + x - 1];
						}
						for (int d = am; d != am + bm; d++){
							inv[((d*maxInv) + c)] = b[(d - am)*bn + y - 1];
						}

						x--;
						y--;
					}

				}
			}

			c++;
		}

		k[0] = c;
	}
	__global__ void invertPSP(int original[], int inverse[], int n, int k[], int mn)
	{
		for (int d = 0; d != n; d++){
			for (int c = 0; c < k[0]; c++){
				if (inverse[(mn*d) + (k[0] - c - 1)] > 0 && inverse[(mn*d) + (k[0] - c)] < 256){
					original[(mn*d) + c] = inverse[(mn*d) + (k[0] - c - 1)];
				}
				else{
					original[(mn*d) + c] = 32;
				}
			}
		}
	}
	int main(){
		return 0;
	}
}

/*  6 febrero 2015
__global__ void alignPSP(int *a, int *b, int *matrix, int *matrixDir, int *scoreMatrix, int am, int an, int bm, int bn, int gap0, int gap1, int gap2, int offset, int size, int *order)
{
//gap0=gap gap, gap1=gap opening, gap2=gap mistmatch

bool flag = 0;
int index = (blockIdx.x*blockDim.x + threadIdx.x) + (offset * size);
int x = index + 1;
int topLeft, left, top;
printf("Soy el index: %d\n",index);
if (index <= an){
order[index] = 0;
for (int c = 0; c != bn + 1; c++){
matrix[c*(an + 1) + x] = 0;
matrixDir[c*(an + 1) + x] = 0;
}
matrixDir[x] = 2;
}

__syncthreads();
__threadfence_block();
if (index <= an){
if (index != 0 && offset == 0){
for (int c = 0; c != bn + 1; c++)
{
matrixDir[c*(an + 1) + index] = c == 0 ? 2 : 5;
}
}
else{
for (int c = 0; c != bn + 1; c++)
{
matrixDir[c*(an + 1)] = 3;
}
matrixDir[0] = 0;
}
}

__syncthreads();
__threadfence_block();
if (index < an){
for (int y = 1; y <= bn; y++){
if (index == 0){
order[0] = y;
flag = 1;
}
else{
//printf("Yo: %d order[%d]=%d\n", index,index-1,order[index-1]);
if (order[index - 1] > y){
flag = 1;
order[index]++;
}
else{
flag = 0;
y--;
}
}
__syncthreads();
__threadfence_block();

if (flag){
int sum = 0;
for (int xx = 0; xx != am; xx++){
for (int yy = 0; yy != bm; yy++){
if ((a[xx*an + (x - 1)] != '-'&&b[yy*bn + (y - 1)] != '-')){
sum += scoreMatrix[((a[xx*an + (x - 1)] - 65) * 27) + (b[yy*bn + (y - 1)] - 65)];
//	printf("Se comparó: %c y %c y salio: %d\n", a[xx*an + (x - 1)], b[yy*bn + (y - 1)], scoreMatrix[((a[xx*an + (x - 1)] - 65) * 27) + (b[yy*bn + (y - 1)] - 65)]);
}
else{
if (a[xx*an + (x - 1)] == b[yy*bn + (y - 1)]){
sum += gap0;
}
else{
sum += gap2;
}
}
}
}

topLeft = matrix[(y - 1)*(an+1) + (x - 1)] + sum;

sum = 0;
for (int xx = 0; xx != am; xx++){
for (int yy = 0; yy != bm; yy++){
if (a[xx*an + (x - 1)] == '-'){
sum += gap0;
}
else{
sum += gap2;
}
}
}
if (y>1)
sum += matrixDir[((y-1)*(an+1)) + x] == 1 ? gap1 : 0;

top = matrix[((y - 1)*(an+1)) + x] + sum;
sum = 0;
for (int xx = 0; xx != am; xx++){
for (int yy = 0; yy != bm; yy++){
if (b[yy*bn + (y - 1)] == '-'){
sum += gap0;
}
else{
sum += gap2;
}
}
}
if (x>1)
sum += matrixDir[y*(an + 1) + x-1] == 1 ? gap1 : 0;
left = matrix[(y*(an+1)) + (x - 1)] + sum;
//printf("matrix[%d,%d] top:%d topleft:%d left:%d\n", x, y, top, topLeft, left);
if (topLeft >= left&&topLeft >= top){
matrixDir[y*(an + 1) + x] = 1;
matrix[y*(an + 1) + x] = topLeft;
//	printf("matrix[%d,%d]=%d,%d\n", x, y, matrix[y*(an + 1) + x], matrixDir[y*(an + 1) + x]);
}
else
if (top > topLeft&&top >= left){
matrixDir[y*(an + 1) + x] = 3;
matrix[y*(an + 1) + x] = top;
//printf("matrix[%d,%d]=%d,%d\n", x, y, matrix[y*(an + 1) + x], matrixDir[y*(an + 1) + x]);
}
else
{
matrixDir[y*(an + 1) + x] = 2;
matrix[y*(an + 1) + x] = left;
//	printf("matrix[%d,%d]=%d,%d\n", x, y, matrix[y*(an + 1) + x], matrixDir[y*(an + 1) + x]);
}
if (y == bn){
order[index]++;
}
}
}

order[index]++;
order[index]++;
__syncthreads();
__threadfence_block();
}
}

*/