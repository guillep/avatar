baselines
baseline: spec

	<baseline>
	
	spec for: #'common' do: [
		spec blessing: #'baseline';   
		repository: 'github://guillep/avatar';  
		
		package: 'Avatar';
		package: 'Avatar-Tests' with: [ spec requires: #('Avatar') ];
		
		group: 'default' with: #('tests');
		group: 'core' with: #('Avatar');
		group: 'tests' with: #('Avatar-Tests')
	].