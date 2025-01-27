"
A ClassifyOnIteratorTest is a test class for testing the behavior of ClassifyOnIterator
"
Class {
	#name : #ClassifyOnIteratorTest,
	#superclass : #IteratorDecoratorTest,
	#category : #'Iterators-Tests-Decorators'
}

{ #category : #'iterator creation' }
ClassifyOnIteratorTest >> createIteratorOn: anObject [
	^ (ClassifyOnIterator classifier: [ :elem | elem ]) decorate: anObject
]

{ #category : #accessing }
ClassifyOnIteratorTest >> iteratorWalk [
	"Returns the walk that is expected to be done by the iterator under test."

	^ (1 to: 10) collect: [ :elem | elem -> (Array with: elem) ]
]

{ #category : #tests }
ClassifyOnIteratorTest >> testOddAndEven [
	| classifier res |
	classifier := (ClassifyOnIterator
		classifier: [ :i | 
			i odd
				ifTrue: [ #odd ]
				ifFalse: [ #even ] ]) decorate: (1 to: 7) iterator.
	res := classifier next.
	self assert: res key equals: #even.	"even sorted before odd"
	self assert: res value size equals: 3.	"2 4 6"
	res := classifier next.
	self assert: res key equals: #odd.
	self assert: res value equals: #(1 3 5 7).
	self assert: classifier hasNext not	"no more please"
]
