class Cell {
}

class EmptyCell extends Cell {
    override def toString() = {
        "empty"
    }
        
}

class NumberCell(number: Int)  extends Cell {
    override def toString() = {
        number.toString()
    }
}

class StringCell(string: String)  extends Cell {
    override def toString() = {
        string
    }
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
    override def toString() = {
        if (ix >= table.width || iy >= table.length || ix < 0 || iy < 0) {
            "outOfRange"
        }
        else {
            val set = Set[Cell]()
            checkCycle(table.getCell(ix, iy), set)
        }
    }

    def checkCycle(optionCell: Option[Cell], setCells: Set[Cell]) : String = {
        optionCell match {
            case Some(cell) => {
                if (setCells.contains(cell)) {
                        "cyclic"
                }
                else if (cell.isInstanceOf[ReferenceCell]) {
                    val refCell = cell.asInstanceOf[ReferenceCell]
                    val x = refCell.ix
                    val y = refCell.iy
                    checkCycle(table.getCell(x, y), setCells + cell) 
                }
                else {
                    cell.toString()
                }
            }
            case None => "outOfRange"
        }
    }    
}