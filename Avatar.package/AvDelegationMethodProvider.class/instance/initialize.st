testing
initialize

	super initialize.
	"We allow to cache up to 100 different methods"
	cache := LRUCache new maximumWeight: 100.