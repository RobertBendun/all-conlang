#include <iostream>
#include <string_view>
#include <optional>
#include <iomanip>
#include <cassert>
#include <vector>
#include <memory>
#include <span>
#include <stack>

struct Word
{
	unsigned arity;
	std::string_view base;
	unsigned position;

	friend std::ostream& operator<<(std::ostream& os, Word const& word)
	{
		switch (word.arity) {
		case 0: return os << "Nil" << std::quoted(word.base);
		case 1: return os << "Mon" << std::quoted(word.base);
		case 2: return os << "Dya" << std::quoted(word.base);
		default: assert(false);
		}
	}
};

std::vector<Word> tokenize(std::string_view text)
{
	std::vector<Word> words;

	auto const source = text;
	while (text.size()) {
		while (text.starts_with(' ')) { text.remove_prefix(1); }

		std::string_view base = text;

		for (; text.size(); text.remove_prefix(1)) {
			static constexpr char ending[] = { 'i', 'e', 'u' };
			static constexpr std::string_view ending_space[] = { "i ", "e ", "u " };
			static_assert(std::size(ending) == std::size(ending_space));

			assert(not text.starts_with(' '));
			for (auto i = 0u; i < std::size(ending); ++i) {
				if ((text.size() == 1 and text[0] == ending[i]) or text.starts_with(ending_space[i])) {
					base = base.substr(0, base.size() - text.size());
					text.remove_prefix(std::min(size_t(2), text.size()));
					words.push_back(Word { .arity = i, .base = base, .position = unsigned(base.data() - source.data()) });
					goto next;
				}
			}
		}

next:
		;
	}
	return words;
}

struct Node
{
	Word word;
	std::shared_ptr<Node> a{}, b{};
};

std::shared_ptr<Node> parse(std::span<Word> &words);
std::shared_ptr<Node> parse_niladic(std::span<Word> &words);
std::shared_ptr<Node> parse_monadic(std::span<Word> &words);
std::shared_ptr<Node> parse_dyadic(std::span<Word> &words);

std::shared_ptr<Node> parse_left(std::span<Word> &words)
{
	if (words.size() && words.front().arity == 0) {
		auto word = words.front();
		words = words.subspan(1);
		return std::make_shared<Node>(Node { .word = word });
	}

	if (words.size() && words.front().arity == 1) {
		auto word = words.front();
		words = words.subspan(1);
		return std::make_shared<Node>(Node { .word = word, .a = parse_left(words) });
	}

	return nullptr;
}

std::shared_ptr<Node> parse_dyadic(std::span<Word> &words)
{
	if (words.size()) {
		auto lhs = parse_left(words);

		if (words.size() && words.front().arity == 2) {
			auto word = words.front();
			words = words.subspan(1);
			return std::make_shared<Node>(Node { .word = word, .a = lhs, .b = parse(words) });
		} else {
			return lhs;
		}
	}
	return nullptr;
}

std::shared_ptr<Node> parse(std::span<Word> &words)
{
	return parse_dyadic(words);
}

void dump(std::shared_ptr<Node> const& node, unsigned indent = 0)
{
	std::cout << std::string(indent * 2, ' ');
	if (node) {
		std::cout << node->word << '\n';
		if (node->word.arity >= 1) dump(node->a, indent+1);
		if (node->word.arity >= 2) dump(node->b, indent+1);
	} else {
		std::cout << "nullptr\n";
	}
}

int main()
{
	std::string_view source = "male hole ini oldu holi nodu hodu alle holi";

	auto words_vec = tokenize(source);
	for (auto const& word : words_vec) {
		std::cout << word.position << '\t' << word << '\n';
	}

	std::span<Word> words = words_vec;
	auto tree = parse(words);
	dump(tree);
}
