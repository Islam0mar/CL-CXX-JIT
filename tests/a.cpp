/*
 *    \file main.cpp
 *
 * Copyright (c) 2020 Islam Omar (io1131@fayoum.edu.eg)
 */

// #include <Eigen/Core>
// #include <Eigen/Eigen>
// #include <Eigen/Eigenvalues>
// #include <Eigen/Geometry>
//
#include <any>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <type_traits>

#ifdef _WIN32
#define CL_CXX_EXPORT_API __declspec(dllexport)
#define CL_CXX_IMPORT_API __declspec(dllimport)
#else
#define CL_CXX_EXPORT_API __attribute__((visibility("default")))
#define CL_CXX_IMPORT_API
#endif

namespace cl_cxx {
// https://stackoverflow.com/questions/81870/is-it-possible-to-print-a-variables-type-in-standard-c/58331141#58331141
namespace type_name {
template <typename T>
constexpr std::string_view WrappedTypeName() {
#ifdef __clang__
  return __PRETTY_FUNCTION__;
#elif defined(__GNUC__)
  return __PRETTY_FUNCTION__;
#elif defined(_MSC_VER)
  return __FUNCSIG__;
#endif
}

class ProbeType;
constexpr std::string_view kProbeTypeName("cl_cxx::type_name::ProbeType");
constexpr std::string_view kProbeTypeNameElaborated(
    "class cl_cxx::type_name::ProbeType");
constexpr std::string_view kProbeTypeNameUsed(
    WrappedTypeName<ProbeType>().find(kProbeTypeNameElaborated) !=
            std::string_view::npos
        ? kProbeTypeNameElaborated
        : kProbeTypeName);

constexpr size_t PrefixSize() {
  return WrappedTypeName<ProbeType>().find(kProbeTypeNameUsed);
}

constexpr size_t SuffixSize() {
  return WrappedTypeName<ProbeType>().length() - PrefixSize() -
         kProbeTypeNameUsed.length();
}

template <typename T>
constexpr std::string_view TypeName() {
  constexpr auto kTypeName = WrappedTypeName<T>();
  return kTypeName.substr(PrefixSize(),
                          kTypeName.length() - PrefixSize() - SuffixSize());
}
}  // namespace type_name

template <typename T>
static inline void GetTypeName() {
  std::cout << std::string(type_name::TypeName<T>()).c_str() << std::endl;
}

namespace detail {
template <typename T>
constexpr bool IsFunction() {
  return std::is_function<
      std::remove_pointer_t<std::remove_reference_t<T>>>::value;
}
template <typename T>
constexpr bool IsPod() {
  return (std::is_trivial<T>::value && std::is_standard_layout<T>::value) &&
         (std::is_class<T>::value);
}

template <typename T>
constexpr bool IsFundamental() {
  return std::is_fundamental<T>::value ||
         (std::is_array<T>::value && IsPod<T>()) || std::is_pointer<T>::value;
}
template <typename T>
constexpr bool IsString() {
  return std::is_same<T, std::string>::value ||
         std::is_same<T, const std::string>::value;
}
template <typename T>
constexpr bool IsClass() {
  return std::is_class<T>::value &&
         !(std::is_same<T, std::string>::value || IsPod<T>());
}

template <typename T>
struct IsFunctional : public std::false_type {};
template <typename T>
struct IsFunctional<std::function<T>> : public std::true_type {};

template <typename T>
static constexpr bool always_false = false;

}  // namespace detail

#define TYPE_CONVERSION_ERROR_MSG                                            \
  "\nNo known conversion to the requested type :( kindly open an issue and " \
  "report this behavior.\n"

template <typename CppT>
struct ConverterToLisp {
  typedef std::conditional_t<
      detail::IsString<CppT>(), char *,
      std::conditional_t<detail::IsClass<CppT>(), void *, CppT>>
      type;
  auto operator()(CppT cpp_value) {
    if constexpr (detail::IsFundamental<CppT>()) {
      return cpp_value;
    } else if constexpr (detail::IsPod<CppT>()) {
      return cpp_value;
    } else if constexpr (detail::IsString<CppT>()) {
      auto str = new char[cpp_value.size() + 1];
      std::copy(cpp_value.c_str(), cpp_value.c_str() + cpp_value.size() + 1,
                str);
      return str;
    } else if constexpr (detail::IsFunction<CppT>()) {
      return cpp_value;
    } else if constexpr (detail::IsClass<CppT>()) {
      // for consistency with string
      auto obj_ptr = new std::any[1]{cpp_value};
      return static_cast<void *>(obj_ptr);
    } else {
      static_assert(detail::always_false<CppT>, TYPE_CONVERSION_ERROR_MSG);
      return 0;
    }
  }
};

template <>
struct ConverterToLisp<void> {
  typedef void type;
};

template <typename CppT>
auto ConvertToLisp(CppT cpp_value) {
  return ConverterToLisp<std::decay_t<CppT>>()(
      std::forward<std::decay_t<CppT>>(cpp_value));
}

template <typename CppT>
struct ConverterToCpp {
  using LispT = typename ConverterToLisp<CppT>::type;
  typedef CppT type;
  auto operator()(LispT lisp_value) {
    if constexpr (detail::IsFundamental<CppT>()) {
      return lisp_value;
    } else if constexpr (detail::IsPod<CppT>()) {
      return lisp_value;
    } else if constexpr (detail::IsString<CppT>()) {
      return std::string(lisp_value);
    } else if constexpr (detail::IsClass<CppT>()) {
      auto obj_any = *(static_cast<std::any *>(lisp_value));
      return std::any_cast<CppT>(obj_any);
    } else {
      static_assert(detail::always_false<CppT>, TYPE_CONVERSION_ERROR_MSG);
      return 0;
    }
  }
};

template <typename CppT, typename LispT>
auto ConvertToCpp(LispT lisp_value) {
  return ConverterToCpp<std::decay_t<CppT>>()(lisp_value);
}
inline void LispError(const char *);

template <typename T, typename... V>
constexpr auto TypeName(std::vector<std::string> *vec);

namespace detail {
template <auto invokable_pointer, typename R, typename... Args>
typename ConverterToLisp<std::decay_t<R>>::type DoApply(
    typename ConverterToLisp<std::decay_t<Args>>::type... args) {
  try {
    if constexpr (std::is_invocable_v<
                      decltype(invokable_pointer),
                      typename ConverterToLisp<std::decay_t<Args>>::type...>) {
      if constexpr (std::is_same_v<
                        typename ConverterToLisp<std::decay_t<R>>::type,
                        void>) {
        return;
      } else {
        return ConvertToLisp(
            std::invoke(invokable_pointer, ConvertToCpp<Args>(args)...));
      }
    } else {
      if constexpr (std::is_same_v<
                        typename ConverterToLisp<std::decay_t<R>>::type,
                        void>) {
        return;
      } else {
        return ConvertToLisp(
            std::invoke(*invokable_pointer, ConvertToCpp(args)...));
      }
    }
  } catch (const std::exception &err) {
    LispError(err.what());
  }
  return typename ConverterToLisp<std::decay_t<R>>::type();
}

template <auto std_func_ptr, typename R, typename... Args>
constexpr auto ResolveInvocable(std::function<R(Args...)> *f) {
  (void)f;
  return &DoApply<std_func_ptr, R, Args...>;
}
template <auto func_ptr, typename R, typename... Args>
constexpr auto ResolveInvocable(R (*p)(Args...)) {
  (void)p;
  return &DoApply<func_ptr, R, Args...>;
}

template <auto mem_func_ptr, typename R, typename CT, typename... Args>
constexpr auto ResolveInvocable(R (CT::*p)(Args...)) {
  (void)p;
  return &DoApply<mem_func_ptr, R, CT, Args...>;
}
template <auto mem_func_ptr, typename R, typename CT, typename... Args>
constexpr auto ResolveInvocable(R (CT::*p)(Args...) const) {
  (void)p;
  return &DoApply<mem_func_ptr, R, CT, Args...>;
}
template <typename LambdaT, LambdaT *lambda_ptr, typename R, typename... Args>
constexpr auto ResolveInvocableLambda(R (LambdaT::*p)(Args...) const) {
  (void)p;
  return &DoApply<lambda_ptr, R, Args...>;
}

/// mutable lambda
template <typename LambdaT, LambdaT *lambda_ptr, typename R, typename... Args>
constexpr auto ResolveInvocableLambda(R (LambdaT::*p)(Args...)) {
  (void)p;
  return &DoApply<lambda_ptr, R, Args...>;
}
template <auto lambda_ptr>
constexpr auto ResolveInvocable(
    std::enable_if_t<
        std::is_class_v<std::remove_pointer_t<decltype(lambda_ptr)>> &&
            !detail::IsFunctional<
                std::remove_pointer_t<decltype(lambda_ptr)>>::value,
        decltype(lambda_ptr)>
        p) {
  (void)p;
  return ResolveInvocableLambda<std::remove_pointer_t<decltype(p)>, lambda_ptr>(
      &std::remove_pointer_t<decltype(p)>::operator());
}
template <auto x>
inline constexpr auto DecayThenResolve() {
  return ResolveInvocable<std::forward<std::decay_t<decltype(x)>>(x)>(
      std::forward<std::decay_t<decltype(x)>>(x));
}
/// type deduction
template <typename T>
static inline void GetTypeName(std::vector<std::string> *vec) {
  static constexpr auto type_name = type_name::TypeName<T>();
  vec->emplace_back(type_name);
}
template <typename R, typename... Args>
struct InvokableTypeName {};

template <typename LambdaT>
struct InvokableTypeName<LambdaT> {
  void operator()(std::vector<std::string> *vec) {
    InvokableTypeName<decltype(&LambdaT::operator())>()(vec);
  }
};

template <typename R, typename... Args>
struct InvokableTypeName<R(Args...)> {
  void operator()(std::vector<std::string> *vec) { TypeName<R, Args...>(vec); }
};

template <typename R, typename... Args>
struct InvokableTypeName<R (*)(Args...)> {
  void operator()(std::vector<std::string> *vec) { TypeName<R, Args...>(vec); }
};

template <typename R, typename CT, typename... Args>
struct InvokableTypeName<R (CT::*)(Args...)> {
  void operator()(std::vector<std::string> *vec) { TypeName<R, Args...>(vec); }
};

template <typename R, typename CT, typename... Args>
struct InvokableTypeName<R (CT::*)(Args...) const> {
  void operator()(std::vector<std::string> *vec) { TypeName<R, Args...>(vec); }
};
template <typename T>
struct InvokableTypeName<std::function<T>> {
  void operator()(std::vector<std::string> *vec) {
    InvokableTypeName<T>()(vec);
  }
};
}  // namespace detail

template <typename T>
auto InvokableTypeName(T lambda, std::vector<std::string> *vec) {
  return detail::InvokableTypeName<decltype(lambda())>()(vec);
}

template <typename T_, typename... V>
constexpr auto TypeName(std::vector<std::string> *vec) {
  using T = std::decay_t<T_>;
  if constexpr (sizeof...(V) != 0) {
    TypeName<T>(vec);
    TypeName<V...>(vec);
  } else {
    detail::GetTypeName<typename ConverterToLisp<T>::type>(vec);
  }
}

/**
 * @brief      import a function, method, lambda, or std_function
 *
 * @details    takes a lambda input such as this:
 *             [&](){return <function/method/lambda/std_function> ;}
 *             and convert it to the address of thunk_function which
 *             takes arguments to cxx function.
 *             when method is imported, you have to pass object as
 *             the first argument.
 *             example 1:
 *             given: int foo(float f){return (int)f;}
 *             import: Import([&]() { return foo; })
 *             that would give the address of the thunk function.
 *             example 2:
 *             given: int foo(float f){return (int)f;}
 *             import: Import([&]() { return foo; })(5.123)
 *             that would execute foo(5.123) and return 5.
 *             could be invoked with Import([&]() { return &foo; })
 *             use cases:
 *             Import([&]() { return &MyClass::foo; })(foo_object, args...)
 *             Import([&]() { return &MyClass::operator(); })(foo_object)
 *             Import([&]() { return []() { return "Hello, World\n"; }; })()
 *
 * @param      lambda class
 *
 * @return     function pointer to the thunk fuction
 */
template <typename T>
auto Import(T lambda) {
  if constexpr (std::is_class_v<decltype(lambda())>) {
    static auto w = lambda();
    static constexpr auto res = detail::DecayThenResolve<&w>();
    return res;
  } else {
    static constexpr auto res = detail::DecayThenResolve<lambda()>();
    return res;
  }
}

struct Registry {
  void (*error_handler)(const char *);
  void (*reg_data_callback)(void *);
  static auto get_registry() {
    static Registry my_reg;
    return &my_reg;
  }
};

inline void LispError(const char *msg) {
  Registry::get_registry()->error_handler(msg);
}
inline void SendMetaData(void *md) {
  Registry::get_registry()->reg_data_callback(md);
}
}  // namespace cl_cxx

/// wrapper for Import function
#define IMPORT(x...) cl_cxx::Import([&]() { return x; })
#define IVOKABLE_TYPE_NAME(x, v) \
  cl_cxx::InvokableTypeName([&]() { return x; }, v)

extern "C" {

typedef struct {
  // could be void*
  void (*thunk_ptr)();
  bool method_p;
  const char **type;  // memory handled in C++
  std::uint8_t type_size;
} MetaData;

CL_CXX_EXPORT_API bool ClCxxInit(void (*error_handler)(const char *),
                                 void (*reg_data_callback)(void *)) {
  try {
    cl_cxx::Registry::get_registry()->error_handler = error_handler;
    cl_cxx::Registry::get_registry()->reg_data_callback = reg_data_callback;
    return true;
  } catch (const std::runtime_error &err) {
    cl_cxx::LispError(err.what());
  }
  return false;
}

CL_CXX_EXPORT_API bool ClCxxDeleteObject(void *ptr, bool is_char) {
  try {
    if (is_char) {
      delete[] static_cast<char *>(ptr);
    } else {
      delete[] static_cast<std::any *>(ptr);
    }
    return true;
  } catch (const std::runtime_error &err) {
    cl_cxx::LispError(err.what());
  }
  return false;
}

CL_CXX_EXPORT_API bool RegisterPackage() {
  static MetaData md;
  try {
    std::vector<std::string> v;
    std::vector<const char *> c_v;
    md.thunk_ptr = IMPORT([]() { 1 + 5; });
    IVOKABLE_TYPE_NAME([]() { 1 + 5; }, &v);
    for (const auto &i : v) {
      c_v.emplace_back(i.c_str());
    }
    md.type = c_v.data();
    md.type_size = c_v.size();
    cl_cxx::SendMetaData(static_cast<void *>(&md));
    v.clear();
    c_v.clear();

    return true;
  } catch (const std::runtime_error &err) {
    cl_cxx::LispError(const_cast<char *>(err.what()));
  }
  return false;
}
}

// TODO: add buffer size to config file
template <typename T>
int fdsdfs(T x, char w);

auto f() { return (1 + 4); }
class C {
 public:
  int x;

  auto hi() { return "Hi\n"; }
};

int main() {
  auto x = std::string();
  // TypeName<std::string>();
  // TypeName<fdsdfs<double>>();
  // TypeName<decltype(fdsdfs<char **>)>();
  // cl_cxx::TypeName<decltype(&fdsdfs<char **>)>();
  int (*wf)(std::string, char) = &fdsdfs<std::string>;
  // cl_cxx::TypeName<decltype(wf)>();
  auto fx = []() mutable { return 1 + 5; };
  // cl_cxx::TypeName<decltype(f)>();
  // cl_cxx::TypeName<decltype(fx)>();
  // cl_cxx::TypeName<decltype(&fdsdfs<s>)>();
  // auto w = []() { return 0; };
  // cl_cxx::TypeName<decltype(w)>();
  // auto xx = &fdsdfs<decltype(w)>;
  // cl_cxx::TypeName<decltype(xx)>();
  // cl_cxx::TypeName<decltype(&fdsdfs<decltype(w)>)>();
  // cl_cxx::TypeName<decltype(&fdsdfs<cl_cxx::type_name::ProbeType>)>();
  // cl_cxx::TypeName<void>();
  // std::cout << (*wf)("asdas", 's') << std::endl;
  std::cout << (void *)IMPORT(&C::hi) << std::endl;
  std::cout << (void *)IMPORT(&C::hi) << std::endl;
  std::cout << (void *)IMPORT(fdsdfs<std::string>) << std::endl;
  // std::cout << (void *)IMPORT(Eigen::Matrix3d::Constant) << std::endl;
  // const Eigen::CwiseNullaryOp<Eigen::internal::scalar_identity_op<double>,
  //                             Eigen::Matrix<double, 3, 3>> (*dds)();
  // dds = &Eigen::Matrix3d::Identity;
  auto hj1 = &fdsdfs<std::string>;
  // std::cout << (void *)cl_cxx::Import(Eigen::Matrix3d::Constant) <<
  // std::endl;
  std::cout << IMPORT(fx)() << std::endl;
  std::cout << IMPORT(fdsdfs<std::string>) << std::endl;
  // std::cout << cl_cxx::detail::DecayThenResolve<&wf>() << std::endl;
  // std::cout << cl_cxx::detail::ResolveInvocable<
  //                  static_cast<int (*)(char, char)>(fdsdfs)>(
  //                  static_cast<int (*)(char, char)>(fdsdfs))
  //           << std::endl;
  // std::cout << IMPORT([]() { return std::string("Islam King"); })()
  //           << std::endl;
  // std::cout << (void *)cl_cxx::ConvertToLisp(f) << std::endl;
  // std::cout << cl_cxx::ConvertToLisp(fx) << std::endl;
  // C cc;
  // cc.x = 100;
  // std::cout << cl_cxx::ConvertToLisp(cc).hi() << std::endl;
  // std::cout << cl_cxx::ConvertToLisp(cc).x << std::endl;
  // // std::cout << cl_cxx::ConvertToLisp(cc) << std::endl;
  // volatile float t1 = 23.9;
  // std::cout << cl_cxx::ConvertToLisp(t1) << std::endl;
  // std::string s = "asdasd";
  // std::cout << cl_cxx::ConvertToLisp(s) << std::endl;
  // std::string &pt1 = s;
  // std::cout << "---\n";
  // // cl_cxx::ConvertToLisp(pt1);
  // std::cout << cl_cxx::ConvertToLisp(pt1) << std::endl;
  // std::cout << (void *)cl_cxx::ConvertToLisp(fdsdfs<char *>) << std::endl;
  // auto xx = &fdsdfs<char *>;
  // std::cout << (void *)&fdsdfs<char *> << std::endl;
  // std::cout << (void *)xx << std::endl;
  // std::function<int(float)> ff = [](float x) { return (int)x; };
  // // std::cout << (void*)&cl_cxx::ConvertToLisp(ff) << std::endl;
  // cl_cxx::ConvertToLisp(ff);
  // cl_cxx::GetTypeName<void (*)(float)>();
  // cl_cxx::GetTypeName<void()>();
  // cl_cxx::GetTypeName<std::string()>();
  std::vector<std::string> v;
  cl_cxx::detail::InvokableTypeName<char (*)(
      int32_t, int16_t, int16_t, char *, void *, size_t, unsigned char, int8_t,
      long long, uint64_t, long)>()(&v);
  // cl_cxx::detail::InvokableTypeName<decltype(&C::hi)>()(&v);
  // v.emplace_back("...\n");
  // cl_cxx::detail::InvokableTypeName<void(float)>()(&v);
  // v.emplace_back("...\n");
  // cl_cxx::detail::InvokableTypeName<std::function<void(float)>>()(&v);
  // v.emplace_back("...\n");
  // auto ppps = []() { return 0; };
  // cl_cxx::detail::InvokableTypeName<decltype(ppps)>()(&v);
  // cl_cxx::detail::InvokableTypeName<std::string(std::function<void(float)>)>()(
  //     &v);
  // std::cout << v.size() << std::endl;
  for (auto x : v) {
    std::cout << x << std::endl;
  }

  return 0;
}
// wrapped.template constructor<const string&>();

template <typename T>
int fdsdfs(T f, char asd) {
  (void)f;
  (void)asd;
  return 0;
}
