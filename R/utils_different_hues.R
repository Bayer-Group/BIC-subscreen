different_hues <- function(hex_code, value = 21) {
  ifelse(
    ((grDevices::col2rgb(hex_code)[1] * 0.299) + (grDevices::col2rgb(hex_code)[2] * 0.587) + (grDevices::col2rgb(hex_code)[3] * 0.114) > 186),
    grDevices::rgb(max(grDevices::col2rgb(hex_code)[1] - value, 0), max(grDevices::col2rgb(hex_code)[2] - value, 0), max(grDevices::col2rgb(hex_code)[3] - value, 0), maxColorValue = 255),
    grDevices::rgb(min(grDevices::col2rgb(hex_code)[1] + value, 255), min(grDevices::col2rgb(hex_code)[2] + value, 255), min(grDevices::col2rgb(hex_code)[3] + value,255), maxColorValue = 255)
  )
}
