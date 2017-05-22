/* 
 * Author: Andrew Best
 * abest@ccrma.stanford.edu
 *
 * FIR Lowpass Filter with Maximum Stop Band Attenuation
 * See FIR Filter Design via Spectral Factorization and Convex Optimization - Wu, Boyd, Vandengerghe
 * and fir_mag_design_low.m - Mutapcic at http://cvxr.com/cvx/examples
 */

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "fir_lowpass.h"
#include "ecos.h"

#define PI 3.14159265358979323846

void printm(qc_matrix *M) {
	printf ("-----------------------------\n");
	for (int i = 0; i < M->m; i++) {
		for (int j = 0; j < M->n; j++) {
			printf("%2ld,%2ld :%.4f ", M->i[i*M->n + j], M->j[i*M->n + j], M->v[i*M->n + j]);
		}
		printf ("\n");
	}
	printf ("-----------------------------\n");
}

int main(void) {
	const int n = 25;
	const int m = 15*n;
	const double wpass = 0.12*PI;
	const double wstop = 0.24*PI;
	const double delta = 1.0;

	fir_lowpass_params *params = new fir_lowpass_params;
	fir_lowpass_dims *dims = new fir_lowpass_dims;

	double *A_v = new double[m*n];
	memset(A_v, 0, m*n*sizeof(double));
	params->A = new qc_matrix;
	params->A->v = A_v;
	params->A->nnz = m*n;
	params->A->m = m;
	params->A->n = n;
	params->A->i = new long[m*n];
	params->A->j = new long[m*n];
	printf("FIR Filter!\n");

	double *w = new double[m];
	// Load w
	for (int i = 0; i < m; i++) {
		w[i] = i*PI/(m-1);
	}

	// Load A
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < n; j++) {
			if (j == 0) {
				params->A->v[i*n + j] = 1.0;
			}
			else {
				params->A->v[i*n + j] = 2*cos(j*w[i]);
			}
			params->A->i[i*n + j] = (long) i;
			params->A->j[i*n + j] = (long) j;
		}
	}

	// Find passband length
	int length;
	for (int i = 0; i < m; i++) {
		if (w[i] < wpass) {
			length = i+1;
		}
	}
	dims->pb = length;

	params->Lp = new double[length];
	params->Up = new double[length];

	params->Ap = new qc_matrix;
	params->Ap->v = A_v;
	params->Ap->nnz = length*n;
	params->Ap->m = length;
	params->Ap->n = n;
	params->Ap->i = new long[length*n];
	params->Ap->j = new long[length*n];

	for (int i = 0; i < length; i++) {
		for (int j = 0; j < n; j++) {
			params->Ap->i[i*n + j] = i;
			params->Ap->j[i*n + j] = j;
		}
	}

	for (int i = 0; i < length; i++) {
		params->Lp[i] = pow(pow(10.0, -delta/20.0), 2.0);
		params->Up[i] = pow(pow(10.0, +delta/20.0), 2.0);
	}

	// Find stopband length
	for (int i = 0; i < m; i++) {
		if (w[i] < wstop) {
			length = m-i-1;
		}
	}
  dims->sb = length;

	params->As = new qc_matrix;
	params->As->v = A_v + m*n - n*length;
	params->As->nnz = length*n;
	params->As->m = length;
	params->As->n = n;
	params->As->i = new long[length*n];
	params->As->j = new long[length*n];

	for (int i = 0; i < length; i++) {
		for (int j = 0; j < n; j++) {
			params->As->i[i*n + j] = i;
			params->As->j[i*n + j] = j;
		}
	}

	qc_socp *data = qc_fir_lowpass2socp(params, dims);

	if (!data) {
		printf("No data!\n");
		return 1;
	}

	// run ecos and solve it
	pwork *mywork = ECOS_setup(data->n, data->m, data->p,
		data->l, data->nsoc, data->q, 0,
		data->Gx, data->Gp, data->Gi,
		data->Ax, data->Ap, data->Ai,
		data->c, data->h, data->b);

	if (mywork)
	{
		ECOS_solve(mywork);
		printf("Objective value at termination of C program is %f\n", mywork->info->pcost);
		ECOS_cleanup(mywork, 0);
	}
	qc_socp_free(data);
  
  return 0;
}
