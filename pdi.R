# Gerekli paketlerin yüklenmesi ve çağrılması
if (!requireNamespace("doremi", quietly = TRUE)) {
  install.packages("doremi")
}
if (!requireNamespace("fda.usc", quietly = TRUE)) {
  install.packages("fda.usc")
}

library(doremi)
library(fda.usc)

calc_pdp_diff <- function(profile1,
                          profile2,
                          method = "pdi",
                          deriv = "gold",
                          gold_spline = TRUE,
                          gold_embedding = 4,
                          nderiv = 4,
                          gold_spline_threshold = 0.01,
                          epsilon = NULL) {
  # profile2'nin x değerleri alınır
  x <- profile2$x
  
  # profile1'in y değerleri profile2'nin x değerlerine göre interpolasyon yapılarak hesaplanır
  calculated_y <-  stats::approx(profile1$x, profile1$y, xout = x, rule = 2)$y
  
  if (method == "pdi") {
    if (deriv == "gold") {
      # profile1 ve profile2'nin türevleri doremi::calculate.gold fonksiyonu kullanılarak hesaplanır
      profile1der <- doremi::calculate.gold(
        time = x,
        signal = calculated_y,
        embedding = gold_embedding,
        n = 1
      )
      profile2der <- doremi::calculate.gold(
        time = x,
        signal = profile2$y,
        embedding = gold_embedding,
        n = 1
      )
      
      if (gold_spline) {
        # Kübik spline derecesi 3 kullanılarak spline interpolasyonu yapılır
        spline1 <- stats::spline(profile1der$dsignal[, 2] ~ profile1der$dtime)
        spline2 <- stats::spline(profile2der$dsignal[, 2] ~ profile2der$dtime)
        
        # gold_spline_threshold ve epsilon değerlerinin altındaki spline değerleri sıfırlanır
        if (!is.null(gold_spline_threshold)) {
          threshold1 <- gold_spline_threshold * max(abs(spline1$y))
          spline1$y[abs(spline1$y) < threshold1] <- 0
          
          threshold2 <- gold_spline_threshold * max(abs(spline2$y))
          spline2$y[abs(spline2$y) < threshold2] <- 0
        }
        
        if (!is.null(epsilon)) {
          spline1$y[abs(spline1$y) < epsilon] <- 0
          spline2$y[abs(spline2$y) < epsilon] <- 0
        }
        
        # İki profilin türevlerinin işaretlerinin farklılık oranı hesaplanır (PDI)
        m <- mean(sign(spline1$y) != sign(spline2$y))
        return(list(
          method = method,
          method_detail = "gold_spline",
          distance = m,
          der1 = spline1$y,
          der2 = spline2$y,
          x = x
        ))
        
      } else {
        # İki profilin türevlerinin işaretlerinin farklılık oranı hesaplanır (PDI)
        m <- mean(sign((profile1der$dsignal[, 2])) != sign((profile2der$dsignal[, 2])), na.rm = TRUE)
        return(list(
          method = method,
          method_detail = "gold",
          distance = m,
          der1 = profile1der$dsignal[, 2],
          der2 = profile2der$dsignal[, 2],
          x = x
        ))
        
      }
    } else if (deriv == "simple") {
      # Basit türev hesaplaması yapılır
      derivative1 <- fda.usc::fdata.deriv(fda.usc::fdata(calculated_y, argvals = x), nderiv = nderiv)$data[1, ]
      derivative2 <- fda.usc::fdata.deriv(fda.usc::fdata(profile2$y, argvals = x), nderiv = nderiv)$data[1, ]
      
      # Epsilon eşik değerinin altındaki türev değerleri sıfırlanır
      if (!is.null(epsilon)) {
        derivative1[abs(derivative1) < epsilon] <- 0
        derivative2[abs(derivative2) < epsilon] <- 0
      }
      
        # İki profilin türevlerinin işaretlerinin farklılık oranı hesaplanır (PDI)
      m <- mean(sign(derivative1) != sign(derivative2))
      return(list(
        method = method,
        method_detail = "simple",
        distance = m,
        der1 = derivative1,
        der2 = derivative2,
        x = x
      ))
      
    } else {
      warning("Lütfen 'simple' veya 'gold' seçeneklerinden birini seçin")
    }
  } else if (method == "L2") {
    # İki profil arasındaki L2 mesafesi hesaplanır
    m <- as.numeric(fda.usc::metric.lp(
      fda.usc::fdata(calculated_y, argvals = x),
      fda.usc::fdata(profile2$y, argvals = x)
    ))
    return(list(method = method, method_detail = "L2", distance = m, x = x))
    
  } else if (method == "L2_derivative") {
    # İki profilin türevleri arasındaki L2 mesafesi hesaplanır
    m <- as.numeric(fda.usc::semimetric.deriv(
      fda.usc::fdata(calculated_y, argvals = x),
      fda.usc::fdata(profile2$y, argvals = x)
    ))
    return(list(method = method, method_detail = "L2_derivative", distance = m, x = x))
  }
}
