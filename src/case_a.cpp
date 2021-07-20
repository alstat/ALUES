#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)
// For more on using Rcpp click the Help button on the editor toolbar

// The following implements the Right Face of all MFs

// [[Rcpp::export]]
List case_a(NumericMatrix df, NumericMatrix score, CharacterMatrix suiClass, double Min, double Max, double mfNum,
            double bias, int j, double a, double b, double c, double l1, double l2, double l3, double l4, double l5, double sigma) {
  int i, w = j - 1;
  int df_row = df.nrow();
  List out(2);
  
  for (i = 0; i < df_row; ++i) {
    // Triangular
    if (mfNum == 1) {
      if ((df(i, w) < Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if (df(i, w) == Min) {
        score(i, w) = 1; suiClass(i, w) = "S1"; 
      } else if ((df(i, w) > Min) && (df(i, w) <= Max)) {
        score(i, w) = (Max - df(i, w)) / (Max - Min);
        if (bias == 1) {
          l1 = 0; l2 = (Max - c) / (Max - Min); l3 = (Max - b) / (Max - Min); l4 = (Max - a) / (Max - Min); l5 = 1;
        }
        if ((score(i, w) >= l1) && (score(i, w) < l2)) {
          suiClass(i, w) = "N";
        } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
          suiClass(i, w) = "S3";
        } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
          suiClass(i, w) = "S2";
        } else if ((score(i, w) >= l4) && (score(i, w) <= l5)) {
          suiClass(i, w) = "S1";
        } else {
          suiClass(i, w) = "NA";
        }
      } else {
        // for capturing bug
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
    // Trapezoidal
    if (mfNum == 2) {
      if ((df(i, w) < Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) >= Min) && (df(i, w) <= a)) {
        score(i, w) = 1; suiClass(i, w) = "S1";
      } else if ((df(i, w) > a) && (df(i, w) <= Max)) {
        score(i, w) = (Max - df(i, w)) / (Max - a);
        if (bias == 1) {
          l1 = 0; l2 = (Max - c) / (Max - a); l3 = (Max - b) / (Max - a); l4 = l5 = 1;
        }
        if ((score(i, w) >= l1) && (score(i, w) < l2)) {
          suiClass(i, w) = "N";
        } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
          suiClass(i, w) = "S3";
        } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
          suiClass(i, w) = "S2";
        } else if (score(i, w) >= l4) {
          suiClass(i, w) = "S1";
        } else {
          suiClass(i, w) = "NA";
        }
      } else {
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    } 
    // Gaussian
    if (mfNum == 3) {
      if (df(i, w) < Min) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if (df(i, w) >= Min) {
        score(i, w) = exp(-pow(((df(i, w) - Min) / sigma), 2)/2.0);
        if (bias == 1) {
          l1 = 0; l2 = exp(-pow(((c - Min) / sigma), 2)/2.0);
          l3 = exp(-pow(((b - Min) / sigma), 2)/2.0);
          l4 = exp(-pow(((a - Min) / sigma), 2)/2.0);
          l5 = 1;
        }
        if ((score(i, w) >= l1) && (score(i, w) < l2)) {
          suiClass(i, w) = "N";
        } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
          suiClass(i, w) = "S3";
        } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
          suiClass(i, w) = "S2";
        } else if ((score(i, w) >= l4) && (score(i, w) <= l5)) {
          suiClass(i, w) = "S1";
        } else {
          suiClass(i, w) = "NA";
        }
      } else {
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
  }
  out[0] = score; out[1] = suiClass;
  return out;
}
