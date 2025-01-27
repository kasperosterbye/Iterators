"
I remember object I encounter coming from the iterator I wrap and filter those that I have already seen.

This, I remove duplicated objects from the wrapped iterator.
"
Class {
	#name : #DuplicateRemoverIterator,
	#superclass : #IteratorDecoratorWithInternalIterator,
	#instVars : [
		'seen'
	],
	#category : #'Iterators-Decorators'
}

{ #category : #accessing }
DuplicateRemoverIterator >> buildInternalIterator [
	^ [ :item | seen add: item ] doIt
			decoratedBy: [ :item | seen includes: item ] rejectIt
]

{ #category : #initialization }
DuplicateRemoverIterator >> initialize [
	super initialize.
	seen := Set new.
]
