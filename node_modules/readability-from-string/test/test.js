import test from 'ava';
import readabilityFromString from '../src';

test('throws without href option', t => {
	t.throws(() => readabilityFromString('<html></html>'));
});

test('it runs readability on given html', t => {
	const html = `
		<html>
			<head>
				<title>My article about stuff and such - My page</title>
			</head>
			<body>
				<h1>My page</h1>
				<article>
					<section>
						<h2>My article about stuff and such</h2>
						<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu.</p>

						<p>In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus.</p>

						<p>Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc,</p>
					</section>
				</article>
			</body>
		</html>
	`;
	const readable = readabilityFromString(html, {href: 'http://example.com'});
	t.ok(readable);
	t.is(readable.title, 'My article about stuff and such');
	t.notOk(/<h[12]>/.test(readable.content), 'The headings should have been removed');
	t.ok(/lorem ipsum/i.test(readable.content), 'The content should still be there');
});
