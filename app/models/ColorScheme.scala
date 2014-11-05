package models

case class ColorScheme(colorId: Int) {

  val foregroundColors = colorId match {
      case 1 => "['#7D8A2E','#C9D787','#7E8AA2','#D8CAA8','#284907','#382513','#468966','#5C832F','#FFB03B','#363942','#B64926','#8E2800','#263248','#FF9800','#FFC0A9','#69D2E7','#A7DBD8','#E0E4CC','#F38630','#FA6900','#FC9D9A','#F9CDAD','#C8C8A9','#83AF9B','#556270','#C7F464','CornflowerBlue', 'Crimson', 'DarkOrange', 'DarkOrchid', 'DarkTurquoise', 'Fuchsia', 'Gold', 'HotPink', 'LightSeaGreen', 'SteelBlue']"
      case 2 => "['#000000']"
      case 3 => "['#FFFFFF']"
  }

  val backgroundColor = colorId match {
    case 1 => "#FFFFFF"
    case 2 => "#FFFFFF"
    case 3 => "#000000"
  }

  val colorFun = colorId match {
    case 1 => "null"
    case 2 => "function(x,y) {if (y > 175) return '#996633'; else return '#479147';}"
    case 3 => "null"
  }

}
