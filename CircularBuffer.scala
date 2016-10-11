class CircularBuffer[T : reflect.ClassTag](val _capacity: Int){
    val _values: Array[T] = Array.ofDim[T](_capacity)

    var _first: Int = 0
    var size: Int = 0

    def apply(index: Int): T = {
        require(index >= 0 && index < size, "Index out of boundary error")
        val i = (_first + index) % _capacity
        _values(i)
    }

    def update(index: Int, value: T): Unit = {
        require(index >= 0 && index < size, "Index out of boundary error")
        val i = (_first + index) % _capacity
        _values(i) = value
    }

    def isEmpty(): Boolean = size == 0
    def pushBack(value: T): Unit = {
        require(size < _capacity, "Buffer already full")
        val index = (_first + size) % _capacity
        _values(index) = value
        size += 1
    }
    def popBack(): Unit = {
        require(size > 0, "Index out of boundary")
        size -= 1
    }
    def popFront(): Unit = {
        require(size > 0, "Index out of boundary")
        _first += 1
        size -= 1
    }
    def front(): T = {
        require(size > 0, "Index out of boundary")
        _values(_first)
    }
    def back(): T = {
        require(size > 0, "Index out of boundary")
        val index = (_first + size - 1) % _capacity
        _values(index)
    }
}

def envolope(time_series: Array[MDVector], window_size: Int): (Array[MDVector], Array[MDVector]) = {
    val size: Int = time_series.length
    val upper: Array[MDVector] = Array.ofDim[MDVector](size)
    val lower: Array[MDVector] = Array.ofDim[MDVector](size)
    val du = new CircularBuffer[Int](2 * window_size + 2)
    val dl = new CircularBuffer[Int](2 * window_size + 2)
    du.pushBack(0)
    dl.pushBack(0)

    for(i <- 1 until size){
        if(i > window_size){
            upper(i - window_size - 1) = time_series(du.front())
            lower(i - window_size - 1) = time_series(dl.front())
        }

        if(time_series(i - 1) < time_series(i)){
            du.popBack()
            while(!du.isEmpty() && time_series(du.back()) < time_series(i))
                du.popBack()
        }else{
            dl.popBack()
            while(!dl.isEmpty() && time_series(i) < time_series(dl.back()))
                dl.popBack()
        }

        du.pushBack(i)
        dl.pushBack(i)

        if(i == 2 * window_size + 1 + du.front())
            du.popFront()
        else if(i == 2 * window_size + 1 + dl.front())
            dl.popFront()
    }

    for(i <- size until (size + window_size)){
        upper(i - window_size - 1) = time_series(du.front())
        lower(i - window_size - 1) = time_series(dl.front())
        if(i - du.front() >= 2 * window_size + 1)
            du.popFront();
        if(i - dl.front() >= 2 * window_size + 1)
            dl.popFront();
    }

    (upper, lower)
}
