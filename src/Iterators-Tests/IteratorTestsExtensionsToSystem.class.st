Class {
	#name : #IteratorTestsExtensionsToSystem,
	#superclass : #TestCase,
	#category : #'Iterators-Tests-ExtensionsToSystem'
}

{ #category : #tests }
IteratorTestsExtensionsToSystem >> testBlockClosureCollectIt [
	| block iterator |
	block := [ :x | x + 1  ].
	iterator := block collectIt.
	
	self assert: iterator class equals: CollectIterator.
	self assert: iterator block equals: block
]

{ #category : #tests }
IteratorTestsExtensionsToSystem >> testBlockClosureRejectIt [
	| block iterator |
	block := [ :x | true ].
	iterator := block rejectIt.
	
	self assert: iterator class equals: SelectIterator.
	self deny: iterator block equals: block.
	
	self assert: (iterator block value: #foo) equals: false. "Ensure that the block is negated."
]

{ #category : #tests }
IteratorTestsExtensionsToSystem >> testBlockClosureSelectIt [
	| block iterator |
	block := [ :x | x = 1 ].
	iterator := block selectIt.
	
	self assert: iterator class equals: SelectIterator.
	self assert: iterator block equals: block
]

{ #category : #tests }
IteratorTestsExtensionsToSystem >> testSymbolCollectIt [
	| symbol iterator |
	symbol := #squared.
	iterator := symbol collectIt.
	
	self assert: iterator class equals: CollectIterator.
	self assert: iterator block equals: symbol
]

{ #category : #tests }
IteratorTestsExtensionsToSystem >> testSymbolRejectIt [
	| symbol iterator |
	symbol := #odd.
	iterator := symbol rejectIt.
	
	self assert: iterator class equals: SelectIterator.
	self deny: iterator block equals: symbol.
	
	self assert: (iterator block value: 1) equals: false. "Ensure that the block is negated."
]

{ #category : #tests }
IteratorTestsExtensionsToSystem >> testSymbolSelectIt [
	| symbol iterator |
	symbol := #odd.
	iterator := symbol selectIt.
	
	self assert: iterator class equals: SelectIterator.
	self assert: iterator block equals: symbol
]
