function Join(data, feedName, windowName) {
	this.data = data;
	this.feedName = feedName;
	this.windowName = windowName;
	this.len = this.data.length;
}

function JoinIterator(join) {
	this.join = join;
	this.current = 0;
	this.row = 0;
}

function checkFeedName(join, current) {
	if (join.feedName == undefined && join.windowName == undefined) {
        	return true;
	} 
	else if (join.data[current][0] == join.feedName && join.data[current][1] == join.windowName) {
		return true;
	}
}

JoinIterator.prototype.next = function() {
	if(this.current >= this.join.data.length) {
		throw StopIteration;
	}
	else {
		if (checkFeedName(this.join, this.current)) {
			if (this.row < this.join.data[this.current][2].length) {
				return this.join.data[this.current][2][this.row++];
			}
			else {
				this.row = 0;
				return this.join[this.current++][2][this.row];
			}
		} 
		else {
			this.current++;
			this.row = 0;
			return undefined;
		}
	}
}

Join.prototype.__iterator__ = function() {
	return new JoinIterator(this);
}

Join.prototype.exists = function (element, position) {
	try {
		for (var i in this) {	
			if (i != undefined) {
				if(i[position] == element) {
					return true;
				}
			}
		}
	}
	catch (StopIteration) {}
	return false;
}