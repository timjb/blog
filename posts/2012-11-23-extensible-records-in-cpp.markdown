---
title: Extensible Records in C++
---

In my view, *extensible records* are a rather good problem for gauging
type system expressivity. I recently wrote an extensible records
framework for Haskell called
[Vinyl](https://github.com/jonsterling/Vinyl); today, I thought I would
see what it would take to manifest something similar in C++.

<!--more-->

> To get started, **extensible records** just means a compositional
> approach to data structures, usually involving some kind of row
> polymorphism. So, given specifications for some record fields, we can
> get an expressive kind of static duck typing.

### The Design

I'd like to make a few specifications about the final product:

1. It should take no more than one line of code to define each field.

2. Types should be locally inferrable.

3. The names of fields should be available without API consumers ever
   dealing with strings.

4. It should be possible to access fields with normal dot-syntax.

### The Implementation

#### Field Representation

First, we'll want to have an idea of what a field is. Let's consider
fields to be structures which are composed into larger structures using
multiple inheritance. So, given a field like the following:

~~~~{.Cpp}
struct address { char const *address; };
~~~~

<!-- *---> We'll want to expose some metadata about it.

~~~~{.Cpp}
template < class F         // the field structure (e.g. address)
         , class T         // the field's type (e.g. const char*)
         , T F::*ptr       // a path to the member (e.g. &address::address)
         , char const *sym // the field's name (e.g. "address")
         >
struct Field {
    typedef T Type;
    static T F::*access()       { return ptr; }
    static char const *symbol() { return sym; }
};
~~~~

Making this metadata is quite simple:

~~~~{.Cpp}
extern char const address_sym[] = "address";
typedef Field<address, char const*, &address::address, address_sym> address_meta;
~~~~

<!-- _--->

> **Note** that we cannot instantiate the string template parameter
> directly in C++; we must provide it in a symbol with external linkage.

The problem now is twofold: first, we don't want the user to have to
enter in all this boilerplate, small as it is; secondly, we don't have any
way of deriving the metadata from the `address` struct itself. So,
something a little bit more clever is called for!

Let's make a static lookup-table:

~~~~{.Cpp}
template <class F> struct FieldTable {};

template <>
struct FieldTable<address>
  : Field<address, const char*, &address::address, address_sym>
{ };
~~~~

And so we will now be able to access metadata for some field `T` using
only `FieldTable<T>`! Now we're getting somewhere. Let's automate that:

~~~~{.Cpp}
#define mk_field(T, sym) \
    extern char const sym##_symbol[] = #sym;\
    struct sym { T sym; };\
    template <> struct FieldTable<sym> : Field<sym, T, &sym::sym, sym##_symbol> {};
~~~~

#### Record Representation

A record is a just a variadic class template, defined inductively:

~~~~{.Cpp}
template <class... rows> struct Record;
~~~~

**The Base Case:** The only thing we can do with an empty record is add
something to it.

~~~~{.Cpp}
template <> struct Record<> {
    Record() { }

    template <class f>
    Record<f> insert(typename FieldTable<f>::Type val) const {
        Record<g> embiggened(*this); // make an enlarged record
        embiggened.*(FieldTable<f>::access()) = val;
        return embiggened;
    }
}
~~~~


**The Inductive Step:** Herein lies the central conceit. We end up
inheriting from every single field in the parameter list; we do this
recursively (by inheriting from `Record` instantiated at the tail of the
current parameter list).

~~~~{.Cpp}
template <class f, class... rows>
struct Record<f, rows...> : f, Record<rows...>
{
~~~~

We need to enlarge records when inserting new fields. This is not
entirely safe, but the presence of `null` in C++ means that it doesn't
make much of a difference.

~~~~{.Cpp}
    Record (const Record<rows...>& smaller) : Record<rows...>(smaller) { }
~~~~

Our implementation for `insert` here is mostly the same as the one for
the empty record; the type's a tad different, though.

~~~~{.Cpp}
    template <class g>
    Record<g,f,rows...> insert(typename FieldTable<g>::Type val) const {
        Record<g,f,rows...> embiggened(*this);
        embiggened.*(FieldTable<g>::access()) = val;
        return embiggened;
    }

~~~~

We allow the construction of a new record with the value to one field
changed.

~~~~{.Cpp}
    template <class g>
    Record set(typename FieldTable<g>::Type val) const {
        Record copy(*this);
        copy.*(FieldTable<g>::access()) = val;
        return copy;
    }
~~~~

We can also just pass in a function to modify the value:

~~~~{.Cpp}
    template <class T>
    using Endomorphism = std::function<T(T)>;

    template <class g>
    Record modify(Endomorphism<typename FieldTable<g>::Type> func) const {
        auto value = func(this->*(FieldTable<g>::access()));
        return set<g>(value);
    }
}
~~~~

### Pretty Printing

The fact that we provide the field's name in our static metadata will
come in handy now! It is the case that all records can be shot through
an `std::ostream`!

~~~~{.Cpp}
std::ostream& operator<<(std::ostream& stream, const Record<>& empty) {
    return stream << "{}";
}

template <class f, class... rows>
std::ostream& operator<<(std::ostream& stream, const Record<f, rows...>& record) {
    Record<rows...> smaller = record;
    typedef FieldTable<f> field;
    return stream << field::symbol() << " : " << record.*(field::access()) << ", " << smaller;
}
~~~~


### Test Drive

We can play with the machinery we built now. Let's first define a few
fields:

~~~~{.Cpp}
mk_field(char const*, name);
mk_field(char const*, favorite_food);
mk_field(int, age);
~~~~

We could even have put these in a namespace for extra safety. We can of
course express a generic function which increments the `age` of any
record which has that field:

~~~~{.Cpp}
template <class R>
R tick_tock(R const& rec) {
    return rec.template modify<age>([](int i) { return i + 1; });
}
~~~~

Let's try making up some records now:

~~~~{.Cpp}
void test_records() {
    const Record<> empty;
    const auto step1 = empty.insert<name>("jon");
    const auto step2 = step1.insert<age>(20);
    const auto step3 = step2.insert<favorite_food>("icecream");
    const auto step4 = step3.set<favorite_food>("calzone");
    const auto step5 = tick_tock(step3);

    std::cout << step1 << std::endl;
    // => name : jon, {}
    std::cout << step2 << std::endl;
    // => age : 20, name : jon, {}
    std::cout << step3 << std::endl;
    // => favorite_food : icecream, age : 20, name : jon, {}
    std::cout << step4 << std::endl;
    // => favorite_food : calzone, age : 20, name : jon, {}
    std::cout << step5.age << std::endl;
    // => 21
}
~~~~


### Next Steps

A few changes would have to be made to our design to allow for
*removing* fields from records nicely: our record representation would
have to be recursive rather than flat for the proper return type to be
readily computable.

It would also be interesting to see how difficult it would be to
derive generic equality for records using `operator==`.

Finally, the most interesting thing to do would be to have some notion
of compositional lenses to allow deep update. For instance, imagine if
the following were possible:

~~~~{.Cpp}
auto changed = man.modify<brother,dog,fur,color>("orange");
~~~~

