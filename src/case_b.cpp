#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)
  
  // For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
List case_b(NumericMatrix df, NumericMatrix score, CharacterMatrix suiClass, double Min, double Max, double mfNum,
            double bias, double wt, int j, double a, double b, double c, double l1, double l2, double l3, double l4, double l5, double sigma) {
  int i, w = j - 1;
  List out(2);
  
  for (i = 0; i < df.nrow(); ++i) {
    // Triangular
    if (mfNum == 1) {
      if ((df(i, w) <= Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) > Min) && (df(i, w) <= Max)) {
        if (wt == 0) {
          score(i, w) = (df(i, w) - Min) / (Max - Min);
          if (bias == 1) {
            l1 = 0; l2 = a / Max; l3 = b / Max; l4 = c / Max; l5 = Max / Max;
          }
          if ((score(i, w) >= l1) && (score(i, w) < l2)) {
            suiClass(i, w) = "N";
          } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
            suiClass(i, w) = "S3";
          } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
            suiClass(i, w) = "S2";
          } else if ((score(i, w) >= l4) && (score(i, w) <= l5)) {
            suiClass(i, w) = "S1";
          }
        } else if (wt > 0) {
          score(i, w) = (df(i, w) - Min) / (Max - Min);
          if (bias == 1) {
            l1 = 0; l2 = a / Max; l3 = b / Max; l4 = c / Max; l5 = Max / Max;
          }
          if ((score(i, w) >= l1) && (score(i, w) < l2)) {
            score(i, w) = wt * ((l2 - l1) / 3);
            suiClass(i, w) = "N";
          } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
            score(i, w) = wt * ((l3 - l2) / 3) + l2;
            suiClass(i, w) = "S3";
          } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
            score(i, w) = wt * ((l4 - l3) / 3) + l3;
            suiClass(i, w) = "S2";
          } else if ((score(i, w) >= l4) && (score(i, w) <= l5)) {
            score(i, w) = wt * ((l5 - l4) / 3) + l4;
            suiClass(i, w) = "S1";
          }
        }
      }
    }
    // Trapezoidal
    if (mfNum == 2) {
      if ((df(i, w) <= Min) || (df(i, w) > Max)) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if ((df(i, w) > Min) && (df(i, w) <= c)) {
        if (wt == 0) {
          score(i, w) = (df(i, w) - Min) / (c - Min);
          if (bias == 1) {
            l1 = 0; l2 = a / c; l3 = b / c; l4 = c / c; l5 = 1;
          }
          if ((score(i, w) >= l1) && (score(i, w) < l2)) {
            suiClass(i, w) = "N";
          } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
            suiClass(i, w) = "S3";
          } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
            suiClass(i, w) = "S2";
          } else if (score(i, w) == l4) {
            suiClass(i, w) = "S1";
          }
        } else if (wt > 0) {
          score(i, w) = (df(i, w) - Min) / (c - Min);
          if (bias == 1) {
            l1 = 0; l2 = a / c; l3 = b / c; l4 = c / c; l5 = 1;
          }
          if ((score(i, w) >= l1) && (score(i, w) < l2)) {
            score(i, w) = wt * ((l2 - l1) / 3);
            suiClass(i, w) = "N";
          } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
            score(i, w) = wt * ((l3 - l2) / 3) + l2;
            suiClass(i, w) = "S3";
          } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
            score(i, w) = wt * ((l4 - l3) / 3) + l3;
            suiClass(i, w) = "S2";
          } else if (score(i, w) == l4) {
            score(i, w) = 1;
            suiClass(i, w) = "S1";
          }
        }
      }
    }
    // Gaussian
    if (mfNum == 3) {
      if (df(i, w) > Max) {
        score(i, w) = 0; suiClass(i, w) = "N";
      } else if (df(i, w) <= Max) {
        if (wt == 0) {
          score(i, w) = exp((- 1 / 2) * pow(((df(i, w) - Max) / sigma), 2));
          if (bias == 1) {
            l1 = 0; l2 = exp((- 1 / 2) * pow(((a - Max) / sigma), 2));
            l3 = exp((- 1 / 2) * pow(((b - Max) / sigma), 2));
            l4 = exp((- 1 / 2) * pow(((c - Max) / sigma), 2));
            l5 = 1;
          }
          if ((score(i, w) >= l1) && (score(i, w) < l2)) {
            suiClass(i, w) = "N";
          } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
            suiClass(i, w) = "S3";
          } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
            suiClass(i, w) = "S2";
          } else if (score(i, w) == l4) {
            suiClass(i, w) = "S1";
          }
        } else if (wt > 0) {
          score(i, w) = exp((- 1 / 2) * pow(((df(i, w) - Max) / sigma), 2));
          if (bias == 1) {
            l1 = 0; l2 = exp((- 1 / 2) * pow(((a - Max) / sigma), 2));
            l3 = exp((- 1 / 2) * pow(((b - Max) / sigma), 2));
            l4 = exp((- 1 / 2) * pow(((c - Max) / sigma), 2));
            l5 = 1;
          }
          if ((score(i, w) >= l1) && (score(i, w) < l2)) {
            score(i, w) = wt * (exp((- 1 / 2) * pow(((a - Max) / sigma), 2)) / 3);
            suiClass(i, w) = "N";
          } else if ((score(i, w) >= l2) && (score(i, w) < l3)) {
            score(i, w) = wt * ((exp((- 1 / 2) * pow(((b - Max) / sigma), 2)) - exp((- 1 / 2) * pow(((a - Max) / sigma), 2))) / 3) + exp((- 1 / 2) * pow(((a - Max) / sigma), 2));
            suiClass(i, w) = "S3";
          } else if ((score(i, w) >= l3) && (score(i, w) < l4)) {
            score(i, w) = wt * ((exp((- 1 / 2) * pow(((c - Max) / sigma), 2)) - exp((- 1 / 2) * pow(((b - Max) / sigma), 2))) / 3) + exp((- 1 / 2) * pow(((b - Max) / sigma), 2));
            suiClass(i, w) = "S2";
          } else if (score(i, w) == l4) {
            score(i, w) = wt * ((1 - exp((- 1 / 2) * pow(((c - Max) / sigma), 2))) / 3) + exp((- 1 / 2) * pow(((c - Max) / sigma), 2));
            suiClass(i, w) = "S1";
          }
        }
      }
    }
  }
  out[0] = score; out[1] = suiClass;
  return out;
}