function y = logreturn(S)
    y = zeros(1,length(S));
    for i = 2:length(y)
        y(i) = log(S(i))-log(S(i-1));
    end
end