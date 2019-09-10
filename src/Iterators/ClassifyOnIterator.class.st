"
I am a decorator which classify the objects in my input according to a classifier block.

The classifier output is an iterator of association objects. The keys in the associations are the results of the classifier block. The values for each key is an iterator of the object which are classified under that key.

The order of keys in the key-iterator is sorted using the #sort method of SequenceableCollection.
The order of the objects in the iterator for a specific key is the oreder they appear in the input stream.

Notice: In most other libraries this class would implement ""group by"" instead of ""classify on"". But for historical reasons groupBy does something else in smalltalk.
"
Class {
	#name : #ClassifyOnIterator,
	#superclass : #IteratorDecorator,
	#category : #'Iterators-Decorators'
}

{ #category : #'instance creation' }
ClassifyOnIterator class >> classifier: classifierBlock [
	^ GeneratorDecorator
		pipe: [ :inIterator :gen | 
			| dict obj keys |
			dict := Dictionary new.
			[ inIterator hasNext ]
				whileTrue: [ | key |
					obj := inIterator next.
					key := classifierBlock value: obj.
					(dict at: key ifAbsentPut: [ OrderedCollection new ]) add: obj ].
			keys := dict keys asOrderedCollection sort.
			keys do: [ :each | gen yield: each -> (dict at: each) asArray ] ]
]
