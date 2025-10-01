class Table(val width: Int, val length: Int) {
    var arrCell= scala.collection.mutable.ArrayBuffer.fill[Cell](width * length)(new EmptyCell())

    def getCell(ix: Int, iy: Int): Option[Cell] = {
        if (width <= ix || length <= iy || ix < 0 || iy < 0) {
            None
        }
        else {
            Option[Cell] (arrCell(iy * width + ix))
        }
    }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        arrCell(iy * width + ix) = cell
    }
}