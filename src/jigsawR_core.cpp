// jigsawR C++ optimizations
// Provides optimized implementations for performance-critical hotspots
// with graceful R fallback when unavailable

#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// =============================================================================
// Priority 1: RNG Batch Generation
// =============================================================================

//' Generate batch of deterministic random numbers
//'
//' Uses sine-based RNG matching the JavaScript jigsaw generator.
//' Much faster than calling R's random() function repeatedly.
//'
//' @param seed Starting seed value
//' @param count Number of random values to generate
//' @param min_val Minimum value (default 0.0)
//' @param max_val Maximum value (default 1.0)
//' @return NumericVector of random values
//' @keywords internal
// [[Rcpp::export]]
NumericVector random_batch_cpp(int seed, int count, double min_val = 0.0, double max_val = 1.0) {
  NumericVector result(count);
  double range = max_val - min_val;
  
  for (int i = 0; i < count; i++) {
    double x = std::sin(static_cast<double>(seed + i)) * 10000.0;
    double r = x - std::floor(x);
    result[i] = min_val + r * range;
  }
  
  return result;
}

// =============================================================================
// Priority 2: Bezier Point Interpolation
// =============================================================================

//' Compute cubic Bezier curve points in batch
//'
//' Vectorized implementation of cubic Bezier interpolation.
//' Computes points along the curve defined by P0, CP1, CP2, P1.
//'
//' @param p0 Start point as c(x, y)
//' @param cp1 First control point as c(x, y)
//' @param cp2 Second control point as c(x, y)
//' @param p1 End point as c(x, y)
//' @param n_points Number of points to generate
//' @return NumericMatrix with x and y columns
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix bezier_batch_cpp(NumericVector p0, NumericVector cp1, 
                                NumericVector cp2, NumericVector p1, 
                                int n_points) {
  NumericMatrix result(n_points, 2);
  
  // Pre-extract coordinates
  double p0x = p0[0], p0y = p0[1];
  double cp1x = cp1[0], cp1y = cp1[1];
  double cp2x = cp2[0], cp2y = cp2[1];
  double p1x = p1[0], p1y = p1[1];
  
  // Compute points along the curve
  for (int i = 0; i < n_points; i++) {
    double t = static_cast<double>(i) / static_cast<double>(n_points - 1);
    double one_minus_t = 1.0 - t;
    
    // Precompute powers
    double one_minus_t_sq = one_minus_t * one_minus_t;
    double one_minus_t_cu = one_minus_t_sq * one_minus_t;
    double t_sq = t * t;
    double t_cu = t_sq * t;
    
    // Cubic Bezier: B(t) = (1-t)³P₀ + 3(1-t)²tCP₁ + 3(1-t)t²CP₂ + t³P₁
    double coef1 = one_minus_t_cu;
    double coef2 = 3.0 * one_minus_t_sq * t;
    double coef3 = 3.0 * one_minus_t * t_sq;
    double coef4 = t_cu;
    
    result(i, 0) = coef1 * p0x + coef2 * cp1x + coef3 * cp2x + coef4 * p1x;
    result(i, 1) = coef1 * p0y + coef2 * cp1y + coef3 * cp2y + coef4 * p1y;
  }
  
  return result;
}

// =============================================================================
// Priority 3: SVG Path Translation
// =============================================================================

//' Translate SVG path coordinates by (dx, dy)
//'
//' Character-by-character parsing without regex for better performance.
//' Handles M, L, C, A, and Z commands.
//'
//' @param path_string SVG path d attribute string
//' @param dx X translation
//' @param dy Y translation
//' @return Translated SVG path string
//' @keywords internal
// [[Rcpp::export]]
std::string svg_translate_cpp(std::string path_string, double dx, double dy) {
  // Early return if no translation needed
  if (dx == 0.0 && dy == 0.0) {
    return path_string;
  }
  
  std::string result;
  result.reserve(path_string.size() + 100);
  
  size_t i = 0;
  size_t n = path_string.size();
  
  while (i < n) {
    char c = path_string[i];
    
    // Skip whitespace
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
      result += c;
      i++;
      continue;
    }
    
    // Handle command letters
    if (c == 'M' || c == 'L') {
      // M/L: translate both x and y
      result += c;
      result += ' ';
      i++;
      
      // Skip whitespace
      while (i < n && (path_string[i] == ' ' || path_string[i] == '\t')) i++;
      
      // Parse x coordinate
      size_t start = i;
      while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
             (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
      double x = std::stod(path_string.substr(start, i - start)) + dx;
      
      // Skip whitespace/comma
      while (i < n && (path_string[i] == ' ' || path_string[i] == ',' || path_string[i] == '\t')) i++;
      
      // Parse y coordinate
      start = i;
      while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
             (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
      double y = std::stod(path_string.substr(start, i - start)) + dy;
      
      char buf[64];
      std::snprintf(buf, sizeof(buf), "%.2f %.2f", x, y);
      result += buf;
      
    } else if (c == 'C') {
      // C: translate all 3 coordinate pairs
      result += c;
      result += ' ';
      i++;
      
      for (int pair = 0; pair < 3; pair++) {
        // Skip whitespace
        while (i < n && (path_string[i] == ' ' || path_string[i] == ',' || path_string[i] == '\t')) i++;
        
        // Parse x
        size_t start = i;
        while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
               (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
        double x = std::stod(path_string.substr(start, i - start)) + dx;
        
        // Skip whitespace/comma
        while (i < n && (path_string[i] == ' ' || path_string[i] == ',' || path_string[i] == '\t')) i++;
        
        // Parse y
        start = i;
        while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
               (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
        double y = std::stod(path_string.substr(start, i - start)) + dy;
        
        char buf[64];
        std::snprintf(buf, sizeof(buf), "%.2f %.2f", x, y);
        result += buf;
        if (pair < 2) result += ' ';
      }
      
    } else if (c == 'A') {
      // A: only translate the endpoint (last 2 numbers), keep radii and flags
      result += c;
      result += ' ';
      i++;
      
      // Skip whitespace
      while (i < n && (path_string[i] == ' ' || path_string[i] == '\t')) i++;
      
      // Copy rx, ry, rotation, large-arc, sweep (5 values unchanged)
      for (int val = 0; val < 5; val++) {
        size_t start = i;
        while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
               (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
        result += path_string.substr(start, i - start);
        result += ' ';
        
        // Skip whitespace/comma
        while (i < n && (path_string[i] == ' ' || path_string[i] == ',' || path_string[i] == '\t')) i++;
      }
      
      // Parse and translate endpoint x
      size_t start = i;
      while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
             (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
      double x = std::stod(path_string.substr(start, i - start)) + dx;
      
      // Skip whitespace/comma
      while (i < n && (path_string[i] == ' ' || path_string[i] == ',' || path_string[i] == '\t')) i++;
      
      // Parse and translate endpoint y
      start = i;
      while (i < n && (path_string[i] == '-' || path_string[i] == '.' || 
             (path_string[i] >= '0' && path_string[i] <= '9'))) i++;
      double y = std::stod(path_string.substr(start, i - start)) + dy;
      
      char buf[64];
      std::snprintf(buf, sizeof(buf), "%.2f %.2f", x, y);
      result += buf;
      
    } else if (c == 'Z') {
      // Z: no coordinates, just copy
      result += c;
      i++;
      
    } else {
      // Unknown character, copy as-is
      result += c;
      i++;
    }
  }
  
  return result;
}
