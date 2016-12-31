'use strict';

const fs = require('fs');
const path = require('path');

fs.readdir('.', (err, files) => {
	if (err) {
		throw err;
	}

	const levelFiles = files.filter(file =>
									path.extname(file) === '.json' &&
									path.basename(file, '.json') !== 'levels');

	const levels = [];

	levelFiles.forEach(file => {
		const level = JSON.parse(fs.readFileSync(file));
		levels.push(level);
	});

	fs.writeFileSync('levels.json', JSON.stringify(levels, null, 2));
});
