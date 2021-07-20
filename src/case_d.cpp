#include <Rcpp.h>
#include <iostream>
using namespace std;
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)
// For more on using Rcpp click the Help button on the editor toolbar

// The following implements cases where there are only 5 specified class limits

// [[Rcpp::export]]
List case_d(NumericMatrix df, NumericMatrix score, CharacterMatrix suiClass, double Min, double Max, double Mid, double mfNum,
            double bias, int j, double a, double b, double c, double d, double l1, double l2, double l3, double l4, double l5, double sigma) {
  int i, w = j - 1;
  List out(2);
  
  for (i = 0; i < df.nrow(); ++i) {
    // Triangular
    if (mfNum == 1) {
      if ((df(i, w) < Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) >= Min) && (df(i, w) <= Mid)) {
        score(i, w) = (df(i, w) - Min) / (Mid - Min);
        if (bias == 1) {
          l1 = 0; l2 = (a - Min) / (Mid - Min); l3 = (b - Min) / (Mid - Min); l4 = (c - Min) / (Mid - Min); l5 = 1;
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
      } else if ((df(i, w) > Mid) && (df(i, w) <= Max)) {
        if (bias == 1) {
          l3 = 0; l4 = (Max - d) / (Max - Mid); l5 = 1;
        }
        score(i, w) = (Max - df(i, w)) / (Max - Mid);
        if ((score(i, w) >= l3) && (score(i, w) < l4)) {
          suiClass(i, w) = "S2";
        } else if ((score(i, w) >= l4) && (score(i, w) <= l5)) {
          suiClass(i, w) = "S1";
        } else {
          suiClass(i, w) = "N";
        }
      } else {
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
    // Trapezoidal
    if (mfNum == 2) {
      if ((df(i, w) < Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) >= Min) && (df(i, w) < c)) {
        score(i, w) = (df(i, w) - Min) / (c - Min);
        if (bias == 0) {
          l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.74; l5 = 1;
        } else if (bias == 1) {
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
      } else if ((df(i, w) >= c) && (df(i, w) <= d)) {
        score(i, w) = 1; suiClass(i, w) = "S1";
      } else if ((df(i, w) > d) && (df(i, w) <= Max)) {
        if (bias == 1) {
          l3 = 0; l4 = 1;
        }
        score(i, w) = (Max - df(i, w)) / (Max - d);
        if ((score(i, w) >= l3) && (score(i, w) < l4)) {
          suiClass(i, w) = "S2";
        } else if (score(i, w) >= l4) {
          suiClass(i, w) = "S1";
        } else {
          suiClass(i, w) = "N";
        }
      } else {
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
    // Gaussian
    if (mfNum == 3) {
      if (df(i, w) <= Mid) {
        score(i, w) = exp(- pow(((df(i, w) - Mid) / sigma), 2)/2.0); 
        if (bias == 1) {
          l1 = 0; l2 = exp(- pow(((a - Mid) / sigma), 2)/2.0);
          l3 = exp(- pow(((b - Mid) / sigma), 2)/2.0);
          l4 = exp(- pow(((c - Mid) / sigma), 2)/2.0);
          l5 = 1;
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
      } else if (df(i, w) > Mid) {
        score(i, w) = exp(- pow(((df(i, w) - Mid) / sigma), 2)/2.0); 
        if (bias == 1) {
          l3 = exp(- pow(((Max - Mid) / sigma), 2)/2.0);
          l4 = exp(- pow(((d - Mid) / sigma), 2)/2.0);
          l5 = 1;
        }
        if ((score(i, w) >= l3) && (score(i, w) < l4)) {
          suiClass(i, w) = "S2";
        } else if ((score(i, w) >= l4) && (score(i, w) <= l5)) {
          suiClass(i, w) = "S1";
        } else {
          suiClass(i, w) = "N";
        }
      } else {
        score(i, w) = -1; suiClass(i, w) = "NA";
      }
    }
  }
  out[0] = score; out[1] = suiClass;
  return out;
}
