#pragma once

#include <cstddef>
#include <concepts>
#include <functional>
#include <type_traits>

namespace glasssix
{
	namespace detail
	{
		template<typename R, typename T, typename... Args>
		struct member_function_wrapper_base
		{
			template<auto Function, typename U>
			static std::function<R(Args...)> make(U&& obj) requires
				std::convertible_to<U, T>&&
				std::is_member_function_pointer_v<decltype(Function)>&&
				std::is_invocable_r_v<R, decltype(std::mem_fn(Function)), U, Args...>
			{
				return[&]<typename... Unbound>(Unbound&&... args)
				{
					return (obj.*Function)(std::forward<Unbound>(args)...);
				};
			}
		};

		template<typename Function>
		struct member_function_wrapper {};

#define MAKE_MEMBER_FUNCTION_WRAPPER(...) \
	template<typename R, typename T, typename... Args> \
	struct member_function_wrapper<R(T::*)(Args...) __VA_ARGS__> : member_function_wrapper_base<R, T, Args...> {}

		MAKE_MEMBER_FUNCTION_WRAPPER();
		MAKE_MEMBER_FUNCTION_WRAPPER(const);
		MAKE_MEMBER_FUNCTION_WRAPPER(volatile);
		MAKE_MEMBER_FUNCTION_WRAPPER(const volatile);
		MAKE_MEMBER_FUNCTION_WRAPPER(&);
		MAKE_MEMBER_FUNCTION_WRAPPER(const&);
		MAKE_MEMBER_FUNCTION_WRAPPER(volatile&);
		MAKE_MEMBER_FUNCTION_WRAPPER(const volatile&);
		MAKE_MEMBER_FUNCTION_WRAPPER(&&);
		MAKE_MEMBER_FUNCTION_WRAPPER(const&&);
		MAKE_MEMBER_FUNCTION_WRAPPER(volatile&&);
		MAKE_MEMBER_FUNCTION_WRAPPER(const volatile&&);
	}

	template<auto Function, typename T>
	auto make_member_function(T&& obj) requires std::is_member_function_pointer_v<decltype(Function)>
	{
		return detail::member_function_wrapper<decltype(Function)>::make<Function>(std::forward<T>(obj));
	}

	template<typename Function>
	struct make_std_function
	{
		using type = decltype(std::function{ std::declval<Function>() });
	};

	template<typename Function>
	using make_std_function_t = typename make_std_function<Function>::type;

	template<typename StdFunction>
	struct std_function_traits {};

	template<typename R, typename... Args>
	struct std_function_traits<std::function<R(Args...)>>
	{
		using return_type = R;

		template<std::size_t I>
		using arg_type = std::tuple_element_t<I, std::tuple<Args...>>;

		static constexpr std::size_t arg_count = sizeof...(Args);
	};

	template<typename Function>
	struct function_traits
	{
		using std_func_type = make_std_function_t<Function>;
		using return_type = typename std_function_traits<std_func_type>::return_type;

		template<std::size_t I>
		using arg_type = typename std_function_traits<std_func_type>::template arg_type<I>;

		static constexpr std::size_t arg_count = std_function_traits<std_func_type>::arg_count;
	};

	template<std::size_t I, typename Function>
	struct function_adapter
	{
		using std_func_type = make_std_function_t<Function>;
		using std_func_traits = std_function_traits<std_func_type>;

		inline static std_func_type std_func;
		static constexpr auto c_func = []<std::size_t... Is>(std::index_sequence<Is...>)
		{
			return[](typename std_func_traits::template arg_type<Is>... args) { return std_func(args...); };
		}(std::make_index_sequence<std_func_traits::arg_count>{});

		template<typename SameFunction>
		constexpr function_adapter(std::integral_constant<std::size_t, I>, SameFunction&& func) noexcept
		{
			std_func = std::forward<SameFunction>(func);
		}
	};

	template<std::size_t I, typename Function>
	function_adapter(std::integral_constant<std::size_t, I>, Function&&)->function_adapter<I, Function>;

#define GET_C_FUNCTION_PTR(func) (decltype(glasssix::function_adapter{ std::integral_constant<std::size_t, __COUNTER__>{}, (func) })::c_func)
}
