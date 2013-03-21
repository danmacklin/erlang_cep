function Join(data){
	this.data = data;
}

function JoinIterator(join){
	this.join = join;
	this.current = 0;
	this.row = 0;
}

JoinIterator.prototype.next = function() {
	if(this.current >= this.join.data.length) {
		throw StopIteration;
	}
	else {
		if (this.row < this.join.data[this.current][2].length){
			return this.join.data[this.current][2][this.row++];
		}
		else {
			this.row = 0;
			return this.join[this.current++][2][this.row];
		}

		return join[this.current++][2][this.row];
	}
};

Join.prototype.__iterator__ = function() {
	return new JoinIterator(this);
};

Join.prototype.exists = function (element, position) {
	try{
		for (var i in this) {			
			if(i[position] == element){
				return true;
			}
		}
	}
	catch (StopIteration) {}
	return false;
};