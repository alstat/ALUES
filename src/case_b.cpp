#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)
// For more on using Rcpp click the Help button on the editor toolbar

// The following implements the Left Face of all MFs

// [[Rcpp::export]]
List case_b(NumericMatrix df, NumericMatrix score, CharacterMatrix suiClass, double Min, double Max, double mfNum,
            double bias, int j, double a, double b, double c, double l1, double l2, double l3, double l4, double l5, double sigma) {
  int i, w = j - 1;
  List out(2);
  
  for (i = 0; i < df.nrow(); ++i) {
    // Triangular
    if (mfNum == 1) {
      if ((df(i, w) < Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) >= Min) && (df(i, w) <= Max)) {
        score(i, w) = (df(i, w) - Min) / (Max - Min);
        if (bias == 1) {
          l1 = 0; l2 = (a - Min) / (Max - Min); l3 = (b - Min) / (Max - Min); l4 = (c - Min) / (Max - Min); l5 = 1;
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
        // to capture bug
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
    // Trapezoidal
    if (mfNum == 2) {
      if ((df(i, w) < Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) >= Min) && (df(i, w) < c)) {
        score(i, w) = (df(i, w) - Min) / (c - Min);
        if (bias == 1) {
          l1 = 0; l2 = (a - Min) / (c - Min); l3 = (b - Min) / (c - Min);  l4 = l5 = 1;
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
      } else if ((df(i, w) >= c) && (df(i, w) <= Max)) {
        score(i, w) = 1; suiClass(i, w) = "S1";
      } else {
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
    // Gaussian
    if (mfNum == 3) {
      if (df(i, w) > Max) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if (df(i, w) <= Max) {
        score(i, w) = exp(-pow(((df(i, w) - Max) / sigma), 2)/2.0);
        if (bias == 1) {
          l1 = 0; l2 = exp(-pow(((a - Max) / sigma), 2)/2.0);
          l3 = exp(-pow(((b - Max) / sigma), 2)/2.0);
          l4 = exp(-pow(((c - Max) / sigma), 2)/2.0);
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