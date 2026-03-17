'use strict';

const FormData = require('form-data');
const index = require('..');

const sendToCoveralls = async (object, cb) => {
  let urlBase = 'https://coveralls.io';
  if (process.env.COVERALLS_ENDPOINT) {
    urlBase = process.env.COVERALLS_ENDPOINT;
  }

  const url = `${urlBase}/api/v1/jobs`;

  if (index.options.stdout) {
    process.stdout.write(JSON.stringify(object));
    cb(null, {statusCode: 200});
  } else {
    try {
      const form = new FormData();
      form.append('json', JSON.stringify(object));
      const response = await new Promise((resolve, reject) => {
        form.submit(url, (err, res) => {
          const bodyData = [];
          if (err) {
            return reject(err);
          }
          res.on('data', data => bodyData.push(data));
          res.on('end', () => {
            resolve({
              statusCode: res.statusCode,
              body: bodyData.join(),
            });
          });
          return;
        });
      });
      cb(null, response);
    } catch (error) {
      cb(error);
    }
  }
};

module.exports = sendToCoveralls;
