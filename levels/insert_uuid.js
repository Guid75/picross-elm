'use strict';

const fs = require('fs');
const path = require('path');
const recursive = require('recursive-readdir');
const uuid = require('uuid/v4');

function ignoreFunc(file, stats) {
	if (stats.isDirectory()) {
		return false;
	}
	return path.extname(file) !== '.json' ||
		path.basename(file) === 'levels.json';
}

function parseValue(key, str, tokens) {
	if (str.startsWith(key + ' ')) {
		const value = str.slice(key.length + 1);
		tokens[key] = value;
	}
}

let index = 5;

function non2Json(nonFilename, nonContent) {
	const lines = nonContent.split('\n');
	const tokens = {};
	lines.forEach(line => {
		if (parseValue('height', line, tokens)) {
			return;
		}
		if (parseValue('width', line, tokens)) {
			return;
		}
		if (parseValue('goal', line, tokens)) {
			return;
		}
		if (parseValue('title', line, tokens)) {
			return;
		}
	});

	const width = parseInt(tokens.width, 10);
	const height = parseInt(tokens.height, 10);
	const dstContent = [];
	let goal = tokens.goal.slice(1, -1);
	for (let h = 0; h < height; h++) {
		const row = goal.slice(h * width, h * width + width);
		dstContent.push(row.split('').map(v => parseInt(v, 10)));
	}
	const dstName = String(index); //tokens.title;
	const dstDescription = tokens.title;
	const dstObj = {
		name: dstName,
		description: dstDescription,
		content: dstContent
	};
	const jsonFile = path.basename(String(index++), '.non') + '.json';
	fs.writeFileSync(path.join('./converted', jsonFile), JSON.stringify(dstObj, null, 2));
}

function searchForUuid(lines) {
	const uuidLineRegexp = /\s*"uuid":/;

	return lines.some(line => uuidLineRegexp.test(line));
}

function insertUuid(fileName) {
	const rawContent = fs.readFileSync(fileName).toString();
	const nameLineRegexp = /(\s*)"name":/;
	let lines = rawContent.split('\n');

	if (searchForUuid(lines)) {
		console.warn(`we found an existing uuid field for ${fileName} so we ignore this file`);
		return;
	}

	let nameIndex;
	let namePrefix;
	if (!lines.some((line, index) => {
		const matches = nameLineRegexp.exec(line);
		if (matches) {
			nameIndex = index;
			namePrefix = matches[1];
			return true;
		}
		return false;
	})) {
		return;
	}

	lines.splice(nameIndex + 1, 0, `${namePrefix}"uuid": "${uuid()}",`);

	fs.writeFileSync(fileName, lines.join('\n'));
}

recursive('.', [ignoreFunc], function (err, files) {
	files.forEach(insertUuid);
});
