from django.shortcuts import render, redirect
from django.contrib.auth import authenticate, login, logout
from django.contrib import messages
from django.http import HttpResponse
# import openpyxl as xl
from .forms import *
# import pandas as pd
# import requests
import socket

Event_Tuple = namedtuple('Event_Tuple', ['date', 'description'])
Dog_Diary = namedtuple('Dog_Diary', ['date', 'bookings'])


def home(request):
    context = {}
    return render(request, "home.html", context)

# -----------------------------
# --------AUTHENTICATION=------
# -----------------------------

def login_user(request):
    if request.method == "POST":
        username = request.POST['username']
        password = request.POST['password']
        user = authenticate(request, username=username, password=password)
        if user is not None:
            login(request, user)
            return redirect('home')
        else:
            messages.success(request, ("Error logging in."))
            return redirect('login')
    else:
        context = {}
        return render(request, 'login.html', context)

def logout_user(request):
    logout(request)
    return redirect("login")

# -----------------------------
# --------UTILITIES------------
# -----------------------------

def downloadpage(request):
    context = {}
    return render(request, 'down_load.html', context)

def downloadexcel(request):
    if not request.user.is_authenticated: return redirect("login")
    if not socket.gethostname() == "Mum_and_Dads": return redirect("notes")

    writer = pd.ExcelWriter('my_data.xlsx', engine='xlsxwriter')
    all_models = \
        [Category, Diary, Note, Quote, Birthday, Dog, Booking, Event, TH, Player, Shopping, Shop, Timer, TimerElement,
         New_Tide, Tide_Date, Weather, Wordle, TennisMatch, TennisGame, General]

    print("All models:", all_models)
    for count, model in enumerate(all_models, 1):
        print("Saving:", model)
        data = model.objects.all()
        df = pd.DataFrame(list(data.values()))
        df.to_excel(writer, sheet_name=f'{model.string_name}', index=False)
    writer.close()

    # Create an HttpResponse object with the Excel file
    response = HttpResponse(open('my_data.xlsx', 'rb').read(), content_type='application/vnd.ms-excel')
    response['Content-Disposition'] = 'attachment; filename="my_data.xlsx"'

    return response

# -----------------------------
# --------NOTES----------------
# -----------------------------

def dogs(request):
    if not request.user.is_authenticated: return redirect("login")
    form = DogForm()

    if request.method == 'POST':
        form = DogForm(request.POST, request.FILES)
        if form.is_valid(): form.save()
    objects = Dog.objects.all()
    objects = sorted(objects, key=lambda d: d.next_booking())
    print("Dogs", objects)
    options = ["Yes", "No", "Limited"]
    show_images = socket.gethostname() == "Mum_and_Dads"
    show_images = False

    count = len(objects)
    context = {'objects': objects, 'title': "Dogs", 'count': count, "form": form, "edit_mode": False, 'people': people(),
               'options':options, 'show_images': show_images}
    return render(request, 'dog.html', context)

def dog_edit(request, id):
    print("A")
    if not request.user.is_authenticated: return redirect("login")
    dog = Dog.objects.get(id=id)
    if request.method == 'POST':
        print("B")
        form = DogForm(request.POST, request.FILES, instance=dog)
        if form.is_valid(): form.save()
        print("C")
        return redirect("dogs")

    form = DogForm(instance=dog)
    objects = Dog.objects.all()
    objects = sorted(objects, key=lambda d: d.next_booking())

    count = len(objects)
    options = ["Yes", "No", "Limited"]
    context = {'objects': objects, 'title': "Dogs", 'count': count, "dog": dog, "edit_mode": True,  'people': people(),
               'options':options, 'form':form}
    return render(request, 'dog.html', context)

def dog_diary(request):
    if not request.user.is_authenticated: return redirect("login")
    general = General.objects.get(name="main")
    today = date.today()

    bookings = Booking.objects.filter(end_date__gte=today).order_by('start_date')
    dog_diary = []
    for x in range(general.dog_diary_days):
        day = today + timedelta(days=x)
        day_bookings = bookings.filter(start_date__lte=day).filter(end_date__gte=day)
        new = Dog_Diary(day, day_bookings)
        dog_diary.append(new)
    general = General.objects.get(name="main")
    durations = [30, 60, 90, 120, 150, 180, 360]

    context = {'dog_diary': dog_diary, 'durations': durations, 'general': general}
    return render(request, 'dog_diary.html', context)



def dog_duration(request, dur):
    general = General.objects.get(name="main")
    general.dog_diary_days = dur
    general.save()
    return redirect('dog_diary')

def booking(request, id):
    if not request.user.is_authenticated: return redirect("login")
    dog = Dog.objects.filter(id=id).first()
    if request.method == 'POST':
        form = BookingForm(request.POST or None)
        form.instance.dog = dog
        if form.is_valid():
            new_booking = form.save()
            return redirect("dogs")
        else:
            context = {"dog": dog, "title": f"Booking for {dog.name}", "form": form}
            return render(request, 'booking.html', context)

    context = {"dog": dog, "title": f"Booking for {dog.name}"}
    return render(request, 'booking.html', context)

# -----------------------------
# --------NOTES----------------
# -----------------------------

def notes(request):
    if not request.user.is_authenticated: return redirect("login")
    form = NoteForm()
    if request.method == 'POST':
        form = NoteForm(request.POST or None)
        if form.is_valid(): form.save()
    object = Note.objects.exclude(parent__isnull=False).first()

    return redirect("note", object.id)

def edit_note(request, id):
    if not request.user.is_authenticated: return redirect("login")
    object = Note.objects.get(id=int(id))
    if request.method == 'POST':
        form = NoteForm(request.POST, instance=object)
        if form.is_valid():
            form.save()
            messages.success(request, "Note saved")
            return redirect("note", object.id)
        else:
            for field in form:
                for error in field.errors:
                    print("Error:", field, error)
    form = NoteForm(instance=object)
    categories = Category.objects.all()
    context = {'object': object, 'categories': categories, 'form': form}
    return render(request, 'note_edit.html', context)

def note(request, id):
    if not request.user.is_authenticated: return redirect("login")
    object = Note.objects.get(id=id)
    if request.method == 'POST':
        form = NoteForm(request.POST or None)
        if form.is_valid():
            new_note = form.save()
        else:
            text = form.cleaned_data['text']
            new_note = Note.objects.create(text=text)
        new_note.parent = object
        new_note.save()
    form_empty = NoteForm()
    form = NoteForm(instance=object)
    object.order_children()
    children = Note.objects.filter(parent=object).order_by('category', 'order')
    # add_order_to_children(object)
    count = len(children)
    categories = Category.objects.all()

    home_pc = socket.gethostname() == "Mum_and_Dads"
    context = {'object': object, 'categories': categories, 'children': children, 'count': count, 'form_empty': form_empty, 'form': form, 'home_pc': home_pc}
    return render(request, 'note.html', context)

def up(request, id):
    return reorder(request, -1, id)

def down(request, id):
    return reorder(request, 1, id)

def reorder(request, dir, id):
    object = Note.objects.filter(id=id).first()
    if object is None:
        pass
    elif dir == 1 and object.order == object.parent.max_child_number():
        pass
    elif dir == -1 and object.order == 1:
        pass
    else:
        if object.order:
            other_object = Note.objects.filter(parent=object.parent, order=object.order + dir).first()
            if other_object:
                other_object.order = object.order
                other_object.save()
            object.order = object.order + dir
        else:
            object.order = object.parent.next_child_number()
        object.save()

    if object and object.parent:
        return redirect("note", str(object.parent.id))
    else:
        return redirect("notes")

def add_order_to_children(object):
    children = Note.objects.filter(parent=object).order_by('category')
    used_numbers = []
    for child in children:
        if child.order is None:
            child.order = object.next_child_number()
            child.save()
        if child.order in used_numbers:
            child.order = object.next_child_number()
            child.save()
        used_numbers.append(child.order)

def delete_note(request, id):
    object = Note.objects.filter(id=id).first()
    if object and object.parent:
        parent = object.parent
    if object:
        object.delete()
    if parent:
        return redirect("note", f"{parent.id}")
    else:
        return redirect("notes")

